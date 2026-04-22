open Core
module Location = Pirx_location.Location
module Type = Pirx_types.Type
module Token = Pirx_lexer.Token
module Lexer = Pirx_lexer.Lexer
module Ast = Pirx_ast.Ast

(* Recursive-descent parser with one-slot lookahead. Raises
   [Lexer.Compile_error] on malformed input; caller is expected to catch
   and report. Covers the M1 subset: function declarations, var-decl,
   assignment, return, expression statements, calls, and primary literals.
   No operators, no control flow, no structs. *)

type t = {
  lexer : Lexer.t;
  mutable buffered : Token.lexeme option;
}

let create lexer = { lexer; buffered = None }

let peek t =
  match t.buffered with
  | Some lx -> lx
  | None ->
    let lx = Lexer.next t.lexer in
    t.buffered <- Some lx;
    lx

let advance t =
  match t.buffered with
  | Some lx -> t.buffered <- None; lx
  | None -> Lexer.next t.lexer

let error loc fmt =
  Printf.ksprintf (fun msg -> raise (Lexer.Compile_error (loc, msg))) fmt

let token_desc = function
  | Token.Tok_eof -> "end of file"
  | Tok_ident s -> Printf.sprintf "identifier %s" s
  | Tok_number s -> Printf.sprintf "number %s" s
  | Tok_string _ -> "string literal"
  | Kw_func -> "keyword 'func'"
  | Kw_var -> "keyword 'var'"
  | Kw_return -> "keyword 'return'"
  | Op_assign -> "'='"
  | Lparen -> "'('"
  | Rparen -> "')'"
  | Lbrace -> "'{'"
  | Rbrace -> "'}'"
  | Comma -> "','"
  | Colon -> "':'"
  | Semicolon -> "';'"

let expect t tok ~what =
  let lx = advance t in
  if Token.equal lx.tok tok then lx
  else error lx.loc "expected %s, got %s" what (token_desc lx.tok)

let expect_ident t ~what =
  let lx = advance t in
  match lx.tok with
  | Tok_ident name -> name, lx.loc
  | other -> error lx.loc "expected %s, got %s" what (token_desc other)

let parse_type t : Type.t =
  let lx = advance t in
  match lx.tok with
  | Tok_ident "int" -> Int
  | Tok_ident "int8" -> Int8
  | Tok_ident "int64" -> Int64
  | Tok_ident "bool" -> Bool
  | Tok_ident "string" -> String
  | Tok_ident "void" -> Void
  | Tok_ident name -> error lx.loc "unknown type '%s'" name
  | other -> error lx.loc "expected type, got %s" (token_desc other)

(* Expression parsing — M1 subset is just primary expressions. *)
let rec parse_expression t : Ast.expr =
  let lx = peek t in
  match lx.tok with
  | Tok_number s ->
    let _ = advance t in
    let n =
      try Int64.of_string s
      with _ -> error lx.loc "invalid integer literal '%s'" s
    in
    Ast.make_expr ~loc:lx.loc (E_int_lit n)
  | Tok_string s ->
    let _ = advance t in
    Ast.make_expr ~loc:lx.loc (E_string_lit s)
  | Tok_ident name ->
    let _ = advance t in
    let next = peek t in
    (match next.tok with
     | Lparen -> parse_call t ~name ~loc:lx.loc
     | _ -> Ast.make_expr ~loc:lx.loc (E_ident name))
  | other -> error lx.loc "expected expression, got %s" (token_desc other)

and parse_call t ~name ~loc =
  let _ = expect t Lparen ~what:"'(' after function name" in
  let rec loop acc =
    let lx = peek t in
    match lx.tok with
    | Rparen -> let _ = advance t in List.rev acc
    | _ ->
      let arg = parse_expression t in
      let lx = peek t in
      (match lx.tok with
       | Comma -> let _ = advance t in loop (arg :: acc)
       | Rparen -> let _ = advance t in List.rev (arg :: acc)
       | other ->
         error lx.loc "expected ',' or ')' in call arguments, got %s"
           (token_desc other))
  in
  let args = loop [] in
  Ast.make_expr ~loc (E_call { name; args })

(* Statements. *)
let parse_var_decl t : Ast.stmt =
  let var_lx = expect t Kw_var ~what:"'var'" in
  let name, _ = expect_ident t ~what:"variable name after 'var'" in
  let typ =
    match (peek t).tok with
    | Colon ->
      let _ = advance t in
      Some (parse_type t)
    | _ -> None
  in
  let init =
    match (peek t).tok with
    | Op_assign ->
      let _ = advance t in
      Some (parse_expression t)
    | _ -> None
  in
  S_var_decl { loc = var_lx.loc; name; typ; init }

let parse_return t : Ast.stmt =
  let ret_lx = expect t Kw_return ~what:"'return'" in
  match (peek t).tok with
  | Semicolon -> S_return { loc = ret_lx.loc; value = None }
  | _ ->
    let value = parse_expression t in
    S_return { loc = ret_lx.loc; value = Some value }

let parse_statement t : Ast.stmt =
  let lx = peek t in
  let stmt =
    match lx.tok with
    | Kw_var -> parse_var_decl t
    | Kw_return -> parse_return t
    | _ ->
      let expr = parse_expression t in
      (match (peek t).tok with
       | Op_assign ->
         let _ = advance t in
         let value = parse_expression t in
         Ast.S_assign { loc = lx.loc; target = expr; value }
       | _ -> S_expr expr)
  in
  let _ = expect t Semicolon ~what:"';' after statement" in
  stmt

let parse_block t : Ast.stmt list =
  let _ = expect t Lbrace ~what:"'{'" in
  let rec loop acc =
    match (peek t).tok with
    | Rbrace -> let _ = advance t in List.rev acc
    | Tok_eof ->
      let lx = peek t in
      error lx.loc "unexpected end of file inside block"
    | _ ->
      let s = parse_statement t in
      loop (s :: acc)
  in
  loop []

let parse_func_args t : (string * Type.t) list =
  let _ = expect t Lparen ~what:"'(' after function name" in
  let rec loop acc =
    match (peek t).tok with
    | Rparen -> let _ = advance t in List.rev acc
    | _ ->
      let name, _ = expect_ident t ~what:"parameter name" in
      let _ = expect t Colon ~what:"':' after parameter name" in
      let typ = parse_type t in
      let arg = (name, typ) in
      (match (peek t).tok with
       | Comma -> let _ = advance t in loop (arg :: acc)
       | Rparen -> let _ = advance t in List.rev (arg :: acc)
       | other ->
         let lx = peek t in
         error lx.loc "expected ',' or ')' in parameter list, got %s"
           (token_desc other))
  in
  loop []

let parse_function t : Ast.func =
  let func_lx = expect t Kw_func ~what:"'func'" in
  let name, _ = expect_ident t ~what:"function name after 'func'" in
  let args = parse_func_args t in
  let _ = expect t Colon ~what:"':' before return type" in
  let ret_type = parse_type t in
  let body = parse_block t in
  {
    loc = func_lx.loc;
    name;
    args;
    ret_type;
    body;
    external_ = false;
  }

let parse lexer : Ast.program =
  let t = create lexer in
  let rec loop acc =
    match (peek t).tok with
    | Tok_eof -> List.rev acc
    | Kw_func ->
      let f = parse_function t in
      loop (f :: acc)
    | other ->
      let lx = peek t in
      error lx.loc "expected 'func' at top level, got %s" (token_desc other)
  in
  let functions = loop [] in
  { functions }
