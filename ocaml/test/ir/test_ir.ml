open Core
module Lexer = Pirx_lexer.Lexer
module Parser = Pirx_parser.Parser
module Typecheck = Pirx_typecheck.Typecheck
module Diag = Pirx_typecheck.Diag
module Desugar = Pirx_desugar.Desugar
module Generator = Pirx_ir.Generator
module Ir_pp = Pirx_ir.Pp

let parse source =
  let lexer = Lexer.create ~filename:"test.pirx" ~source in
  Parser.parse lexer

let check program =
  let program, diag = Typecheck.check program in
  if not (Diag.is_empty diag) then
    Alcotest.fail (Printf.sprintf "typecheck errors: %d" (List.length (Diag.entries diag)));
  program

let ir_of_string source =
  let program = parse source in
  let program = check program in
  let program = Desugar.run program in
  Generator.generate program

let test_empty () =
  let ir = ir_of_string "func main(): int {\n    return 0;\n}\n" in
  let expected =
    "Function Pirx_Main (0 locals, 0 temps):\n\
     \   0  Return4(0)\n\
     \n\
     Function Pirx_Init (0 locals, 0 temps):\n\
     \   0  Return()\n\
     \n\
     Function main (0 locals, 1 temps):\n\
     \   0  Call(Pirx_Init())\n\
     \   1  Call4($ret = Pirx_Main())\n\
     \   2  ExternalReturn4($ret)\n\
     \n"
  in
  Alcotest.(check string) "empty main IR" expected (Ir_pp.string_of_program ir)

let test_putchar () =
  let ir = ir_of_string "func main(): int {\n    putchar(65);\n    return 0;\n}\n" in
  let s = Ir_pp.string_of_program ir in
  Alcotest.(check bool) "contains putchar" true
    (String.is_substring s ~substring:"ExternalCall0( = putchar(65/4))")

let test_string_literal () =
  let ir = ir_of_string "func main(): int {\n    printf(\"hello world!\\n\");\n    return 0;\n}\n" in
  let s = Ir_pp.string_of_program ir in
  Alcotest.(check bool) "contains PirxString" true
    (String.is_substring s ~substring:"PirxString(13/4, \"hello world!\\n\"/8))");
  Alcotest.(check bool) "contains PirxPrintf" true
    (String.is_substring s ~substring:"ExternalCall0( = PirxPrintf($1/16))")

let test_function_calls () =
  let source =
    "func first(x: int, y: int) : int {\n\
     \    return x;\n\
     }\n\
     func second(x: int, y: int) : int {\n\
     \    return y;\n\
     }\n\
     func main(): int {\n\
     \    var x: int = first(65, 66);\n\
     \    var y: int = second(65, 66);\n\
     \    putchar(x);\n\
     \    putchar(y);\n\
     \    return 0;\n\
     }\n"
  in
  let ir = ir_of_string source in
  let s = Ir_pp.string_of_program ir in
  Alcotest.(check bool) "contains first call" true
    (String.is_substring s ~substring:"Call4($1 = first(65/4, 66/4))");
  Alcotest.(check bool) "contains second call" true
    (String.is_substring s ~substring:"Call4($2 = second(65/4, 66/4))");
  Alcotest.(check bool) "contains assign x" true
    (String.is_substring s ~substring:"Assign4(x, $1)");
  Alcotest.(check bool) "contains assign y" true
    (String.is_substring s ~substring:"Assign4(y, $2)")

let test_swap () =
  let source =
    "func main(): int {\n\
     \    var x: int = 65;\n\
     \    var y: int = 66;\n\
     \    var z: int = x;\n\
     \    x = y;\n\
     \    y = z;\n\
     \    putchar(x);\n\
     \    putchar(y);\n\
     \    return 0;\n\
     }\n"
  in
  let ir = ir_of_string source in
  let s = Ir_pp.string_of_program ir in
  Alcotest.(check bool) "contains Assign x 65" true
    (String.is_substring s ~substring:"Assign4(x, 65)");
  Alcotest.(check bool) "contains Assign y z" true
    (String.is_substring s ~substring:"Assign4(y, z)");
  Alcotest.(check bool) "no temps" true
    (String.is_substring s ~substring:"Function Pirx_Main (3 locals, 0 temps):")

let test_param_shadow () =
  let source =
    "func f(x: int): int {\n\
     \    var x: int = 2;\n\
     \    return x;\n\
     }\n\
     func main(): int {\n\
     \    return f(1);\n\
     }\n"
  in
  let ir = ir_of_string source in
  let s = Ir_pp.string_of_program ir in
  Alcotest.(check bool) "shadowed local gets unique name" true
    (String.is_substring s ~substring:"Assign4(x#1, 2)");
  Alcotest.(check bool) "return uses unique name" true
    (String.is_substring s ~substring:"Return4(x#1)");
  Alcotest.(check bool) "local count excludes arg" true
    (String.is_substring s ~substring:"Function f (1 locals, 0 temps):")

let () =
  Alcotest.run "ir"
    [
      ( "generate",
        [
          Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "putchar" `Quick test_putchar;
          Alcotest.test_case "string_literal" `Quick test_string_literal;
          Alcotest.test_case "function_calls" `Quick test_function_calls;
          Alcotest.test_case "swap" `Quick test_swap;
          Alcotest.test_case "param_shadow" `Quick test_param_shadow;
        ] );
    ]
