open Core
module Type = Pirx_types.Type

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

let rec pp_expr buf (e : Ast.expr) =
  match e.kind with
  | E_int_lit n -> Buffer.add_string buf (Int64.to_string n)
  | E_string_lit s ->
    Buffer.add_char buf '"';
    Buffer.add_string buf (escape_string s);
    Buffer.add_char buf '"'
  | E_ident name -> Buffer.add_string buf name
  | E_call { name; args } ->
    Buffer.add_char buf '(';
    Buffer.add_string buf name;
    List.iter args ~f:(fun a ->
      Buffer.add_char buf ' ';
      pp_expr buf a);
    Buffer.add_char buf ')'

let pp_stmt buf = function
  | Ast.S_var_decl { name; typ; init; _ } ->
    Buffer.add_string buf "(decl ";
    Buffer.add_string buf name;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (match typ with
      | Some t -> Type.to_string t
      | None -> "inferred");
    (match init with
     | None -> ()
     | Some e -> Buffer.add_char buf ' '; pp_expr buf e);
    Buffer.add_char buf ')'
  | S_assign { target; value; _ } ->
    Buffer.add_string buf "(= ";
    pp_expr buf target;
    Buffer.add_char buf ' ';
    pp_expr buf value;
    Buffer.add_char buf ')'
  | S_expr e -> pp_expr buf e
  | S_return { value = None; _ } -> Buffer.add_string buf "(return)"
  | S_return { value = Some e; _ } ->
    Buffer.add_string buf "(return ";
    pp_expr buf e;
    Buffer.add_char buf ')'

let pp_func buf (f : Ast.func) =
  Buffer.add_char buf '(';
  if f.external_ then Buffer.add_string buf "extern func "
  else Buffer.add_string buf "func ";
  Buffer.add_string buf f.name;
  Buffer.add_string buf " (";
  List.iteri f.args ~f:(fun i (name, typ) ->
    if i > 0 then Buffer.add_char buf ' ';
    Buffer.add_char buf '(';
    Buffer.add_string buf name;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (Type.to_string typ);
    Buffer.add_char buf ')');
  Buffer.add_string buf "): ";
  Buffer.add_string buf (Type.to_string f.ret_type);
  if not f.external_ then begin
    Buffer.add_string buf " (block";
    List.iter f.body ~f:(fun s ->
      Buffer.add_char buf ' ';
      pp_stmt buf s);
    Buffer.add_char buf ')'
  end;
  Buffer.add_char buf ')'

let string_of_program (p : Ast.program) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(program";
  List.iter p.functions ~f:(fun f ->
    Buffer.add_char buf ' ';
    pp_func buf f);
  Buffer.add_char buf ')';
  Buffer.contents buf
