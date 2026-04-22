open Core
module Lexer = Pirx_lexer.Lexer
module Parser = Pirx_parser.Parser
module Pp = Pirx_ast.Pp

(* Pin the printer output on two representative fixtures. Catches accidental
   printer drift; full per-test checks happen via the end-to-end testrunner. *)

let parse_and_print source =
  let l = Lexer.create ~filename:"<test>" ~source in
  let prog = Parser.parse l in
  Pp.string_of_program prog

let test_000 () =
  let source = "func main(): int {\n  return 0;\n}\n" in
  Alcotest.(check string) "000_empty"
    "(program (func main (): int (block (return 0))))"
    (parse_and_print source)

let test_004 () =
  let source = "func main(): int {\n  printf(\"hello world!\\n\");\n  return 0;\n}\n" in
  Alcotest.(check string) "004_string_literal"
    "(program (func main (): int (block (printf \"hello world!\\n\") (return 0))))"
    (parse_and_print source)

let test_parse_error_expected_semicolon () =
  let source = "func main(): int {\n  return 0\n}\n" in
  let l = Lexer.create ~filename:"<test>" ~source in
  try
    let _ = Parser.parse l in
    Alcotest.fail "expected Compile_error for missing ';'"
  with Lexer.Compile_error (_, msg) ->
    Alcotest.(check bool) "mentions ';'" true
      (String.is_substring msg ~substring:"';'")

let () =
  Alcotest.run "parser"
    [ ("pp",
       [ Alcotest.test_case "000_empty"        `Quick test_000
       ; Alcotest.test_case "004_string"       `Quick test_004
       ])
    ; ("errors",
       [ Alcotest.test_case "missing semicolon" `Quick test_parse_error_expected_semicolon
       ])
    ]
