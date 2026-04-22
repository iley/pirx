module Ast = Pirx_ast.Ast

type t

(* Build the function table by seeding with [Builtins.protos] and folding in
   every function in [program]. Last write wins — a user function shadows a
   builtin of the same name, and a duplicate user function silently
   overwrites the earlier one. Matches Go semantics. [diag] is threaded but
   currently unused; reserved so later stages can add reporting without a
   signature change. *)
val build : Ast.program -> Diag.t -> t

val lookup : t -> string -> Proto.t option
