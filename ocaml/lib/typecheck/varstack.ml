open Core
module Type = Pirx_types.Type

(* A stack of frames. Each frame is an assoc list — O(n) lookup, but scopes
   are small and the code is small, matching the Go version's approach. *)
type t = { mutable frames : (string * Type.t) list list }

let create () = { frames = [ [] ] }

let push_scope t = t.frames <- [] :: t.frames

let pop_scope t =
  match t.frames with
  | [] -> failwith "Varstack.pop_scope: no scope to pop"
  | _ :: rest -> t.frames <- rest

let declare t name typ =
  match t.frames with
  | [] -> failwith "Varstack.declare: no scope"
  | top :: rest ->
    if List.Assoc.mem top ~equal:String.equal name
    then `Duplicate_in_scope
    else begin
      t.frames <- ((name, typ) :: top) :: rest;
      `Ok
    end

let lookup t name =
  List.find_map t.frames ~f:(fun frame ->
    List.Assoc.find frame ~equal:String.equal name)
