open Core
module Token = Pirx_lexer.Token
module Lexer = Pirx_lexer.Lexer
module Location = Pirx_location.Location

let pp_token fmt t =
  Format.pp_print_string fmt (Sexp.to_string (Token.sexp_of_t t))

let token_testable = Alcotest.testable pp_token Token.equal

let tokens_of source =
  let l = Lexer.create ~filename:"<test>" ~source in
  let rec loop acc =
    let lx = Lexer.next l in
    match lx.tok with
    | Token.Tok_eof -> List.rev acc
    | t -> loop (t :: acc)
  in
  loop []

let check_tokens ~name source expected =
  Alcotest.(check (list token_testable)) name expected (tokens_of source)

let test_empty () =
  check_tokens ~name:"empty input" "" []

let test_identifier () =
  check_tokens ~name:"single ident" "main_2" [ Tok_ident "main_2" ]

let test_keywords () =
  check_tokens ~name:"keywords"
    "func var return"
    [ Kw_func; Kw_var; Kw_return ]

let test_number () =
  check_tokens ~name:"number" "65" [ Tok_number "65" ]

let test_string_escapes () =
  check_tokens ~name:"string with escapes"
    {|"hello\n\t\"\\"|}
    [ Tok_string "hello\n\t\"\\" ]

let test_punctuation () =
  check_tokens ~name:"punctuation"
    "(){},:;"
    [ Lparen; Rparen; Lbrace; Rbrace; Comma; Colon; Semicolon ]

let test_assign () =
  check_tokens ~name:"assign" "=" [ Op_assign ]

let test_comment_skip () =
  check_tokens ~name:"comment skip"
    "// ignored\nreturn"
    [ Kw_return ]

let test_location_tracking () =
  let l = Lexer.create ~filename:"<test>"
            ~source:"func\n  main" in
  let t1 = Lexer.next l in
  let t2 = Lexer.next l in
  Alcotest.(check int) "func line" 1 t1.loc.line;
  Alcotest.(check int) "func col"  1 t1.loc.col;
  Alcotest.(check int) "main line" 2 t2.loc.line;
  Alcotest.(check int) "main col"  3 t2.loc.col

(* Fixture sources are inlined to avoid relying on the dune sandbox
   layout. Keep in sync with tests/000_empty.pirx and tests/003_swap.pirx. *)

let test_file_000 () =
  check_tokens ~name:"000_empty"
    "func main(): int {\n  return 0;\n}\n"
    [ Kw_func; Tok_ident "main"; Lparen; Rparen; Colon; Tok_ident "int";
      Lbrace; Kw_return; Tok_number "0"; Semicolon; Rbrace ]

let test_file_003 () =
  check_tokens ~name:"003_swap"
    ("func main(): int {\n"
     ^ "  var x: int = 65; // 'A'\n"
     ^ "  var y: int = 66; // 'B'\n"
     ^ "  var z: int = x;\n"
     ^ "  x = y;\n"
     ^ "  y = z;\n"
     ^ "  putchar(x);\n"
     ^ "  putchar(y);\n"
     ^ "  return 0;\n"
     ^ "}\n")
    [ Kw_func; Tok_ident "main"; Lparen; Rparen; Colon; Tok_ident "int"; Lbrace;
      Kw_var; Tok_ident "x"; Colon; Tok_ident "int"; Op_assign; Tok_number "65"; Semicolon;
      Kw_var; Tok_ident "y"; Colon; Tok_ident "int"; Op_assign; Tok_number "66"; Semicolon;
      Kw_var; Tok_ident "z"; Colon; Tok_ident "int"; Op_assign; Tok_ident "x"; Semicolon;
      Tok_ident "x"; Op_assign; Tok_ident "y"; Semicolon;
      Tok_ident "y"; Op_assign; Tok_ident "z"; Semicolon;
      Tok_ident "putchar"; Lparen; Tok_ident "x"; Rparen; Semicolon;
      Tok_ident "putchar"; Lparen; Tok_ident "y"; Rparen; Semicolon;
      Kw_return; Tok_number "0"; Semicolon;
      Rbrace ]

let test_unterminated_string () =
  let l = Lexer.create ~filename:"<test>" ~source:{|"oops|} in
  try
    let _ = Lexer.next l in
    Alcotest.fail "expected Compile_error for unterminated string"
  with Lexer.Compile_error (_, msg) ->
    Alcotest.(check bool) "message mentions unterminated"
      true
      (String.is_substring msg ~substring:"unterminated")

let test_unexpected_char () =
  let l = Lexer.create ~filename:"<test>" ~source:"@" in
  try
    let _ = Lexer.next l in
    Alcotest.fail "expected Compile_error for '@'"
  with Lexer.Compile_error (_, msg) ->
    Alcotest.(check bool) "message mentions unexpected"
      true
      (String.is_substring msg ~substring:"unexpected")

let () =
  Alcotest.run "lexer"
    [ ("tokens",
       [ Alcotest.test_case "empty"             `Quick test_empty
       ; Alcotest.test_case "identifier"        `Quick test_identifier
       ; Alcotest.test_case "keywords"          `Quick test_keywords
       ; Alcotest.test_case "number"            `Quick test_number
       ; Alcotest.test_case "string escapes"    `Quick test_string_escapes
       ; Alcotest.test_case "punctuation"       `Quick test_punctuation
       ; Alcotest.test_case "assign"            `Quick test_assign
       ; Alcotest.test_case "comment skip"      `Quick test_comment_skip
       ; Alcotest.test_case "location tracking" `Quick test_location_tracking
       ])
    ; ("fixtures",
       [ Alcotest.test_case "tests/000_empty"   `Quick test_file_000
       ; Alcotest.test_case "tests/003_swap"    `Quick test_file_003
       ])
    ; ("errors",
       [ Alcotest.test_case "unterminated str"  `Quick test_unterminated_string
       ; Alcotest.test_case "unexpected char"   `Quick test_unexpected_char
       ])
    ]
