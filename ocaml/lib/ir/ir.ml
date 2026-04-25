open Core

type arg =
  | Var of string
  | Int_lit of int64
  | Float_lit of float
  | String_lit of string
  | Zero
[@@deriving sexp_of]

type call_arg = {
  arg : arg;
  size : int;
  is_float : bool;
}
[@@deriving sexp_of]

type op =
  | Assign of { target : string; value : arg; size : int }
  | Call of { result : string option; func : string; args : call_arg list; size : int }
  | ExternalCall of { result : string option; func : string; args : call_arg list; named_args : int; size : int }
  | Return of { value : arg option; size : int }
  | ExternalReturn of { value : arg option; size : int }
[@@deriving sexp_of]

type func = {
  name : string;
  args : string list;
  arg_sizes : int list;
  ops : op list;
}
[@@deriving sexp_of]

type program = { functions : func list } [@@deriving sexp_of]
