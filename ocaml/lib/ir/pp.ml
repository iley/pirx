open Core
open Ir

let escape_string s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.is_print c && Char.to_int c < 127 -> Buffer.add_char buf c
    | c -> Buffer.add_string buf (Printf.sprintf "\\x%02X" (Char.to_int c)));
  Buffer.contents buf

let string_of_arg = function
  | Var name -> name
  | Int_lit n -> Int64.to_string n
  | Float_lit f -> Printf.sprintf "%g" f
  | String_lit s -> Printf.sprintf "\"%s\"" (escape_string s)
  | Zero -> "0"

let string_of_call_arg ca =
  let suffix = if ca.is_float then "f" else "" in
  Printf.sprintf "%s%s/%d" (string_of_arg ca.arg) suffix ca.size

let string_of_call_args args =
  String.concat ~sep:", " (List.map args ~f:string_of_call_arg)

let string_of_op (op : op) =
  match op with
  | Assign { target; value; size } ->
    Printf.sprintf "Assign%d(%s, %s)" size target (string_of_arg value)
  | Call { result = None; func; args; _ } ->
    Printf.sprintf "Call(%s(%s))" func (string_of_call_args args)
  | Call { result = Some r; func; args; size } ->
    Printf.sprintf "Call%d(%s = %s(%s))" size r func (string_of_call_args args)
  | ExternalCall { result; func; args; size; _ } ->
    let r = Option.value result ~default:"" in
    Printf.sprintf "ExternalCall%d(%s = %s(%s))" size r func (string_of_call_args args)
  | Return { value = None; _ } ->
    "Return()"
  | Return { value = Some v; size } ->
    Printf.sprintf "Return%d(%s)" size (string_of_arg v)
  | ExternalReturn { value = None; _ } ->
    "ExternalReturn()"
  | ExternalReturn { value = Some v; size } ->
    Printf.sprintf "ExternalReturn%d(%s)" size (string_of_arg v)

let get_target (op : op) =
  match op with
  | Assign { target; _ } -> target
  | Call { result = Some r; _ } -> r
  | ExternalCall { result = Some r; _ } -> r
  | _ -> ""

let count_locals_and_temps (f : Ir.func) : int * int =
  let args = String.Set.of_list f.args in
  let rec loop ops locals temps =
    match ops with
    | [] -> (Set.length locals, Set.length temps)
    | op :: rest ->
      let target = get_target op in
      if String.is_empty target || String.is_prefix target ~prefix:"@" || Set.mem args target then
        loop rest locals temps
      else if String.is_prefix target ~prefix:"$" then
        loop rest locals (Set.add temps target)
      else
        loop rest (Set.add locals target) temps
  in
  loop f.ops String.Set.empty String.Set.empty

let string_of_func (f : Ir.func) =
  let buf = Buffer.create 256 in
  let locals, temps = count_locals_and_temps f in
  Printf.bprintf buf "Function %s (%d locals, %d temps):\n" f.name locals temps;
  List.iteri f.ops ~f:(fun i op ->
    Printf.bprintf buf "%4d  %s\n" i (string_of_op op));
  Buffer.contents buf

let string_of_program (p : Ir.program) =
  let buf = Buffer.create 512 in
  List.iter p.functions ~f:(fun f ->
    Buffer.add_string buf (string_of_func f);
    Buffer.add_char buf '\n');
  Buffer.contents buf
