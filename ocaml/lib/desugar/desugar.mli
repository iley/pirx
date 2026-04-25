(* Walk [program] and rewrite sugar forms. Currently the only rule:
   [E_string_lit s] → [E_call "PirxString" [E_int_lit len; E_string_lit s]].
   Must run after typecheck so that [E_string_lit] nodes already carry
   [typ = Type.String]. *)
val run : Pirx_ast.Ast.program -> Pirx_ast.Ast.program
