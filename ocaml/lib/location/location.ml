open Core

type t = { file : string; line : int; col : int }
[@@deriving sexp, compare, equal]

let to_string { file; line; col } = Printf.sprintf "%s:%d:%d" file line col
