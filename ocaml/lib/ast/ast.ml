open Core
module Location = Pirx_location.Location
module Type = Pirx_types.Type

type expr = {
  kind : expr_kind;
  loc : Location.t;
  mutable typ : Type.t;
}
and expr_kind =
  | E_int_lit of int64
  | E_string_lit of string
  | E_ident of { mutable name : string }
  | E_call of { name : string; args : expr list }
[@@deriving sexp_of]

type var_decl = {
  loc : Location.t;
  mutable name : string;
  typ : Type.t option;
  init : expr option;
}
[@@deriving sexp_of]

type stmt =
  | S_var_decl of var_decl
  | S_assign of { loc : Location.t; target : expr; value : expr }
  | S_expr of expr
  | S_return of { loc : Location.t; value : expr option }
[@@deriving sexp_of]

type func = {
  loc : Location.t;
  name : string;
  args : (string * Type.t) list;
  ret_type : Type.t;
  body : stmt list;
  external_ : bool;
}
[@@deriving sexp_of]

type program = { functions : func list } [@@deriving sexp_of]

let make_expr ~loc kind = { kind; loc; typ = Type.Undefined }
