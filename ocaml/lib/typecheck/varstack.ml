open Core
module Type = Pirx_types.Type

type binding = {
  unique_name : string;
  typ : Type.t;
}

type t = {
  mutable frames : (string * binding) list list;
  usage_counts : int String.Table.t;
}

let create () =
  { frames = [ [] ]; usage_counts = String.Table.create () }

let push_scope t = t.frames <- [] :: t.frames

let pop_scope t =
  match t.frames with
  | [] -> failwith "Varstack.pop_scope: no scope to pop"
  | _ :: rest -> t.frames <- rest

let next_unique_name t name =
  let count =
    match Hashtbl.find t.usage_counts name with
    | Some n -> n
    | None -> 0
  in
  Hashtbl.set t.usage_counts ~key:name ~data:(count + 1);
  if count = 0 then name
  else Printf.sprintf "%s#%d" name count

let declare t name typ =
  match t.frames with
  | [] -> failwith "Varstack.declare: no scope"
  | top :: rest ->
    if List.Assoc.mem top ~equal:String.equal name
    then `Duplicate_in_scope
    else begin
      let unique_name = next_unique_name t name in
      let entry = (name, { unique_name; typ }) in
      t.frames <- (entry :: top) :: rest;
      `Ok unique_name
    end

let lookup t name =
  List.find_map t.frames ~f:(fun frame ->
    List.Assoc.find frame ~equal:String.equal name)
