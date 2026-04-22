open Core
module Location = Pirx_location.Location

type t =
  | Tok_eof
  | Tok_ident of string
  | Tok_number of string
  | Tok_string of string
  (* keywords *)
  | Kw_func
  | Kw_var
  | Kw_return
  (* operators *)
  | Op_assign
  (* punctuation *)
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Comma
  | Colon
  | Semicolon
[@@deriving sexp, equal]

type lexeme = { tok : t; loc : Location.t } [@@deriving sexp]

let keyword_of_string = function
  | "func" -> Some Kw_func
  | "var" -> Some Kw_var
  | "return" -> Some Kw_return
  | _ -> None
