open Core
module Lexer = Pirx_lexer.Lexer
module Parser = Pirx_parser.Parser
module Ast = Pirx_ast.Ast
module Type = Pirx_types.Type
module Typecheck = Pirx_typecheck.Typecheck
module Diag = Pirx_typecheck.Diag

(* Positive cases: parse + typecheck a fixture, expect an empty diag plus a
   specific expression somewhere in the tree to carry the expected type.
   Negative cases: expect at least one diag entry whose message contains a
   distinguishing substring. *)

let parse_source source =
  let l = Lexer.create ~filename:"<test>" ~source in
  Parser.parse l

let check source =
  let prog = parse_source source in
  Typecheck.check prog

let diag_msgs (d : Diag.t) =
  List.map (Diag.entries d) ~f:(fun e -> e.msg)

let assert_no_errors label (d : Diag.t) =
  if not (Diag.is_empty d) then
    Alcotest.failf "%s: expected no diagnostics, got: %s"
      label (String.concat ~sep:" | " (diag_msgs d))

let assert_diag_contains label substring (d : Diag.t) =
  let msgs = diag_msgs d in
  let any_match = List.exists msgs ~f:(String.is_substring ~substring) in
  if not any_match then
    Alcotest.failf "%s: expected diagnostic containing %S, got: %s"
      label substring (String.concat ~sep:" | " msgs)

(* Walk a program, returning the [typ] of the first expression whose kind
   matches [pred]. Ordering is the natural AST walk. *)
let rec find_expr_type_in_expr pred (e : Ast.expr) =
  if pred e then Some e.typ
  else match e.kind with
    | E_call { args; _ } -> List.find_map args ~f:(find_expr_type_in_expr pred)
    | _ -> None

let find_expr_type_in_stmt pred (s : Ast.stmt) =
  match s with
  | S_var_decl { init = Some e; _ } -> find_expr_type_in_expr pred e
  | S_var_decl { init = None; _ } -> None
  | S_assign { target; value; _ } ->
    (match find_expr_type_in_expr pred target with
     | Some t -> Some t
     | None -> find_expr_type_in_expr pred value)
  | S_expr e -> find_expr_type_in_expr pred e
  | S_return { value = Some e; _ } -> find_expr_type_in_expr pred e
  | S_return { value = None; _ } -> None

let find_expr_type prog pred =
  List.find_map prog.Ast.functions ~f:(fun f ->
    List.find_map f.body ~f:(find_expr_type_in_stmt pred))

(* --- Positive cases ---------------------------------------------------- *)

let test_000_empty () =
  let src = "func main(): int {\n  return 0;\n}\n" in
  let prog, diag = check src in
  assert_no_errors "000" diag;
  (* The [return 0] expression should be typed Int. *)
  let t = find_expr_type prog (fun e ->
    match e.kind with E_int_lit _ -> true | _ -> false)
  in
  Alcotest.(check bool) "return literal is Int" true
    (Option.value_map t ~default:false ~f:(Type.equal Type.Int))

let test_002_function_calls () =
  let src =
    "func first(x: int, y: int) : int { return x; }\n\
     func second(x: int, y: int) : int { return y; }\n\
     func main(): int {\n\
     \  var x: int = first(65, 66);\n\
     \  var y: int = second(65, 66);\n\
     \  putchar(x);\n\
     \  putchar(y);\n\
     \  return 0;\n\
     }\n"
  in
  let prog, diag = check src in
  assert_no_errors "002" diag;
  (* [first(...)] call result should be typed Int. *)
  let t = find_expr_type prog (fun e ->
    match e.kind with E_call { name = "first"; _ } -> true | _ -> false)
  in
  Alcotest.(check bool) "first(...) is Int" true
    (Option.value_map t ~default:false ~f:(Type.equal Type.Int))

let test_003_swap () =
  let src =
    "func main(): int {\n\
     \  var x: int = 65;\n\
     \  var y: int = 66;\n\
     \  var z: int = x;\n\
     \  x = y;\n\
     \  y = z;\n\
     \  putchar(x);\n\
     \  putchar(y);\n\
     \  return 0;\n\
     }\n"
  in
  let _prog, diag = check src in
  assert_no_errors "003" diag

let test_006_printf_variadic () =
  let src =
    "func main(): int {\n\
     \  var x: int;\n\
     \  var y: int;\n\
     \  var z: int;\n\
     \  x = 1;\n\
     \  y = 2;\n\
     \  z = 3;\n\
     \  printf(\"x = %d, y = %d, z = %d\", x, y, z);\n\
     \  return 0;\n\
     }\n"
  in
  let prog, diag = check src in
  assert_no_errors "006" diag;
  let t = find_expr_type prog (fun e ->
    match e.kind with E_call { name = "printf"; _ } -> true | _ -> false)
  in
  Alcotest.(check bool) "printf(...) is Void" true
    (Option.value_map t ~default:false ~f:(Type.equal Type.Void))

(* --- Negative cases ---------------------------------------------------- *)

let test_err_undeclared_ident () =
  let src = "func main(): int {\n  return notdeclared;\n}\n" in
  let _prog, diag = check src in
  assert_diag_contains "undeclared" "notdeclared" diag

let test_err_arity_mismatch () =
  let src = "func main(): int {\n  putchar();\n  return 0;\n}\n" in
  let _prog, diag = check src in
  assert_diag_contains "arity" "putchar" diag

let test_err_type_mismatch_init () =
  let src = "func main(): int {\n  var x: int = \"hi\";\n  return 0;\n}\n" in
  let _prog, diag = check src in
  assert_diag_contains "init mismatch" "expected int" diag

let test_err_return_missing_value () =
  let src = "func main(): int {\n  return;\n}\n" in
  let _prog, diag = check src in
  assert_diag_contains "return missing value" "must return" diag

let test_param_shadowed_by_local () =
  (* Body-local [var x] shadows the param of the same name. Go accepts this
     because checkFunction opens a param scope and checkBlock opens another
     scope for the body. *)
  let src =
    "func f(x: int): int {\n\
     \  var x: int = 1;\n\
     \  return x;\n\
     }\n\
     func main(): int { return 0; }\n"
  in
  let _prog, diag = check src in
  assert_no_errors "param shadowed by local" diag

let test_user_func_shadows_builtin () =
  (* User-defined [putchar] overwrites the builtin silently. Matches Go's
     last-write-wins semantics in the function table. *)
  let src =
    "func putchar(ch: int): int { return ch; }\n\
     func main(): int { putchar(65); return 0; }\n"
  in
  let _prog, diag = check src in
  assert_no_errors "user func shadows builtin" diag

let test_err_duplicate_var () =
  let src =
    "func main(): int {\n\
     \  var x: int = 1;\n\
     \  var x: int = 2;\n\
     \  return 0;\n\
     }\n"
  in
  let _prog, diag = check src in
  assert_diag_contains "duplicate var" "already declared" diag

let () =
  Alcotest.run "typecheck"
    [ ("positive",
       [ Alcotest.test_case "000_empty"          `Quick test_000_empty
       ; Alcotest.test_case "002_function_calls" `Quick test_002_function_calls
       ; Alcotest.test_case "003_swap"           `Quick test_003_swap
       ; Alcotest.test_case "006_printf"         `Quick test_006_printf_variadic
       ])
    ; ("scope",
       [ Alcotest.test_case "param shadowed by local" `Quick test_param_shadowed_by_local
       ; Alcotest.test_case "user shadows builtin"    `Quick test_user_func_shadows_builtin
       ])
    ; ("negative",
       [ Alcotest.test_case "undeclared ident"   `Quick test_err_undeclared_ident
       ; Alcotest.test_case "arity mismatch"     `Quick test_err_arity_mismatch
       ; Alcotest.test_case "init type mismatch" `Quick test_err_type_mismatch_init
       ; Alcotest.test_case "return missing val" `Quick test_err_return_missing_value
       ; Alcotest.test_case "duplicate var"      `Quick test_err_duplicate_var
       ])
    ]
