open Core
module Location = Pirx_location.Location

type t = {
  filename : string;
  source : string;
  len : int;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

exception Compile_error of Location.t * string

let create ~filename ~source =
  { filename; source; len = String.length source; pos = 0; line = 1; col = 1 }

let current_loc t = { Location.file = t.filename; line = t.line; col = t.col }

let peek t = if t.pos >= t.len then None else Some t.source.[t.pos]

let peek_at t i =
  let p = t.pos + i in
  if p >= t.len then None else Some t.source.[p]

let advance t =
  let c = t.source.[t.pos] in
  t.pos <- t.pos + 1;
  match c with
  | '\n' -> t.line <- t.line + 1; t.col <- 1
  | _ -> t.col <- t.col + 1

let rec next t : Token.lexeme =
  skip_whitespace_and_comments t;
  let loc = current_loc t in
  match peek t with
  | None -> { tok = Tok_eof; loc }
  | Some c when Char.is_alpha c || Char.equal c '_' -> lex_ident t ~loc
  | Some c when Char.is_digit c -> lex_number t ~loc
  | Some '"' -> advance t; lex_string t ~loc
  | Some '=' -> advance t; { tok = Op_assign; loc }
  | Some '(' -> advance t; { tok = Lparen; loc }
  | Some ')' -> advance t; { tok = Rparen; loc }
  | Some '{' -> advance t; { tok = Lbrace; loc }
  | Some '}' -> advance t; { tok = Rbrace; loc }
  | Some ',' -> advance t; { tok = Comma; loc }
  | Some ':' -> advance t; { tok = Colon; loc }
  | Some ';' -> advance t; { tok = Semicolon; loc }
  | Some c ->
    raise (Compile_error (loc, Printf.sprintf "unexpected character %C" c))

and skip_whitespace_and_comments t =
  match peek t with
  | Some c when Char.is_whitespace c -> advance t; skip_whitespace_and_comments t
  | Some '/' when Option.equal Char.equal (peek_at t 1) (Some '/') ->
    advance t; advance t;
    skip_line_comment t;
    skip_whitespace_and_comments t
  | _ -> ()

and skip_line_comment t =
  match peek t with
  | None -> ()
  | Some '\n' -> advance t
  | Some _ -> advance t; skip_line_comment t

and lex_ident t ~loc : Token.lexeme =
  let start = t.pos in
  let rec loop () =
    match peek t with
    | Some c when Char.is_alphanum c || Char.equal c '_' -> advance t; loop ()
    | _ -> ()
  in
  loop ();
  let text = String.sub t.source ~pos:start ~len:(t.pos - start) in
  match Token.keyword_of_string text with
  | Some kw -> { tok = kw; loc }
  | None -> { tok = Tok_ident text; loc }

and lex_number t ~loc : Token.lexeme =
  let start = t.pos in
  let rec loop () =
    match peek t with
    | Some c when Char.is_digit c -> advance t; loop ()
    | _ -> ()
  in
  loop ();
  let text = String.sub t.source ~pos:start ~len:(t.pos - start) in
  { tok = Tok_number text; loc }

and lex_string t ~loc : Token.lexeme =
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek t with
    | None -> raise (Compile_error (loc, "unterminated string literal"))
    | Some '"' -> advance t
    | Some '\\' ->
      advance t;
      (match peek t with
       | None -> raise (Compile_error (loc, "unterminated string literal"))
       | Some c -> advance t; Buffer.add_char buf (decode_escape c));
      loop ()
    | Some c ->
      advance t;
      Buffer.add_char buf c;
      loop ()
  in
  loop ();
  { tok = Tok_string (Buffer.contents buf); loc }

(* Matches the Go lexer: unknown escape passes the character through
   literally (backslash dropped). *)
and decode_escape = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | c -> c
