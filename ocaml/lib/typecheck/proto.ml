open Core
module Type = Pirx_types.Type

(* Function signature used by the typechecker. Covers both user-defined and
   builtin functions. [external_name] is the symbol to call at the C ABI
   boundary — falls back to [name] when None. Unused by S3.1's checker but
   carried so later stages (IR-gen) don't need a second table. *)
type t = {
  name : string;
  external_name : string option;
  args : (string * Type.t) list;
  ret_type : Type.t;
  variadic : bool;
  external_ : bool;
}
[@@deriving sexp_of]
