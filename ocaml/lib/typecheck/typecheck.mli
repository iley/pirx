module Ast = Pirx_ast.Ast

(* Walk [program], fill [typ] on every expression, and return the program
   plus a [Diag.t] accumulator of every error found. Types are mutated in
   place — the returned program is the same value. On any error, the
   affected expression's [typ] is set to [Type.Undefined] and the walk
   continues, so one run surfaces as many errors as possible. *)
val check : Ast.program -> Ast.program * Diag.t
