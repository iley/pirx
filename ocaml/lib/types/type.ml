open Core

type t =
  | Int
  | Int8
  | Int64
  | Bool
  | String
  | Void
  | Undefined
  | Pointer of t
  | Slice of t
[@@deriving sexp, equal]

let rec to_string = function
  | Int -> "int"
  | Int8 -> "int8"
  | Int64 -> "int64"
  | Bool -> "bool"
  | String -> "string"
  | Void -> "void"
  | Undefined -> "undefined"
  | Pointer t -> "*" ^ to_string t
  | Slice t -> "[]" ^ to_string t
