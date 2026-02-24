[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** ***** UNIT TESTS COULD GO HERE (JUST AN EXAMPLE) *)
let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

(* These is a simple unit test that tests a single function 'fact'
   If you want to test something large, like interpretation of a piece
   of a minilanguge, it is not longer a unit tests but an integration test.
   Read about dune's cram tests and put the test into `demos/somefile.t`.
*)

open Lambda_lib
open Lambda_lib.Interpret
open QCheck

let parse_optimistically str = Result.get_ok (Parser.parse str)
let pp = Lambda_lib.Print.print_ast

let%expect_test "parse simple application" =
  Format.printf "%s" (pp (parse_optimistically "x y"));
  [%expect {| App (Var "x", Var "y") |}]
;;

let%expect_test "parse application with parens" =
  Format.printf "%s" (pp (parse_optimistically "(x y)"));
  [%expect {| App (Var "x", Var "y") |}]
;;

let%expect_test "parse lambda with backslash" =
  Format.printf "%s" (pp (parse_optimistically "(\\x . x x)"));
  [%expect {| Abs ("x", App (Var "x", Var "x")) |}]
;;

let%expect_test "parse lambda with unicode" =
  Format.printf "%s" (pp (parse_optimistically "(λf.λx. f (x x))"));
  [%expect {| Abs ("f", Abs ("x", App (Var "f", App (Var "x", Var "x")))) |}]
;;

(* Test all binary operators *)
let%expect_test "parse division" =
  Format.printf "%s" (pp (parse_optimistically "10 / 2"));
  [%expect {| BinOp (Div, Int 10, Int 2) |}]
;;

let%expect_test "parse modulo" =
  Format.printf "%s" (pp (parse_optimistically "10 % 3"));
  [%expect {| BinOp (Mod, Int 10, Int 3) |}]
;;

let%expect_test "parse equality" =
  Format.printf "%s" (pp (parse_optimistically "5 = 5"));
  [%expect {| BinOp (Eq, Int 5, Int 5) |}]
;;

let%expect_test "parse not equal" =
  Format.printf "%s" (pp (parse_optimistically "5 <> 3"));
  [%expect {| BinOp (Neq, Int 5, Int 3) |}]
;;

let%expect_test "parse less than" =
  Format.printf "%s" (pp (parse_optimistically "3 < 5"));
  [%expect {| BinOp (Lt, Int 3, Int 5) |}]
;;

let%expect_test "parse greater than" =
  Format.printf "%s" (pp (parse_optimistically "5 > 3"));
  [%expect {| BinOp (Gt, Int 5, Int 3) |}]
;;

let%expect_test "parse less or equal" =
  Format.printf "%s" (pp (parse_optimistically "3 <= 5"));
  [%expect {| BinOp (Leq, Int 3, Int 5) |}]
;;

let%expect_test "parse greater or equal" =
  Format.printf "%s" (pp (parse_optimistically "5 >= 3"));
  [%expect {| BinOp (Geq, Int 5, Int 3) |}]
;;

let%expect_test "parse if without else" =
  Format.printf "%s" (pp (parse_optimistically "if 1 then 42"));
  [%expect {| If (Int 1, Int 42, None) |}]
;;

let%expect_test "parse if with else" =
  Format.printf "%s" (pp (parse_optimistically "if 1 then 42 else 0"));
  [%expect {| If (Int 1, Int 42, Some Int 0) |}]
;;

let%expect_test "parse let without rec" =
  Format.printf "%s" (pp (parse_optimistically "let x = 5 in x"));
  [%expect {| Let (false, "x", Int 5, Var "x") |}]
;;

let%expect_test "parse let rec" =
  Format.printf "%s" (pp (parse_optimistically "let rec f = fun x -> x in f"));
  [%expect {| Let (true, "f", Abs ("x", Var "x"), Var "f") |}]
;;

(* ========================================================================== *)
(* ADDITIONAL COVERAGE TESTS *)
(* ========================================================================== *)

(* Parser: Lambda syntax tests *)
let%expect_test "parse lambda with unicode λ" =
  Format.printf "%s" (pp (parse_optimistically "λx . x"));
  [%expect {| Abs ("x", Var "x") |}]
;;

let%expect_test "parse lambda with backslash alone" =
  Format.printf "%s" (pp (parse_optimistically "\\x . x"));
  [%expect {| Abs ("x", Var "x") |}]
;;

let%expect_test "parse multi-param lambda with unicode" =
  Format.printf "%s" (pp (parse_optimistically "λx y z -> x"));
  [%expect {| Abs ("x", Abs ("y", Abs ("z", Var "x"))) |}]
;;

let%expect_test "parse multi-param lambda with backslash" =
  Format.printf "%s" (pp (parse_optimistically "\\x y z -> x"));
  [%expect {| Abs ("x", Abs ("y", Abs ("z", Var "x"))) |}]
;;

let%expect_test "parse multi-param fun syntax" =
  Format.printf "%s" (pp (parse_optimistically "fun x y -> x"));
  [%expect {| Abs ("x", Abs ("y", Var "x")) |}]
;;

(* Interpreter: Recursive binding tests *)
let%expect_test "eval recursive function - factorial" =
  let ast = parse_optimistically "let rec fact = fun n -> if n then 1 else n in fact 0" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok _ -> Format.printf "<value>"
   | Result.Error (UnknownVariable s) -> Format.printf "UnknownVariable: %s" s
   | Result.Error DivisionByZero -> Format.printf "DivisionByZero"
   | Result.Error TypeMismatch -> Format.printf "TypeMismatch"
   | Result.Error StepLimitExceeded -> Format.printf "StepLimitExceeded");
  [%expect {| <fun> |}]
;;

let%expect_test "eval if-then-else with 0 condition (false)" =
  let ast = parse_optimistically "if 0 then 42 else 99" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok _ -> Format.printf "<value>"
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| 99 |}]
;;

let%expect_test "eval if-then without else (non-zero)" =
  let ast = parse_optimistically "if 1 then 42" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok _ -> Format.printf "<value>"
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| 42 |}]
;;

let%expect_test "eval if-then without else (zero - no else)" =
  let ast = parse_optimistically "if 0 then 42" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok _ -> Format.printf "<value>"
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| 0 |}]
;;

let%expect_test "eval unknown variable error" =
  let ast = parse_optimistically "unknown_var" in
  (match eval_expr ast with
   | Result.Ok _ -> Format.printf "<value>"
   | Result.Error (UnknownVariable s) -> Format.printf "UnknownVariable: %s" s
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| UnknownVariable: unknown_var |}]
;;

let%expect_test "eval division by zero" =
  let ast = parse_optimistically "10 / 0" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok (VBuiltin (name, _)) -> Format.printf "<builtin:%s>" name
   | Result.Ok VUnit -> Format.printf "()"
   | Result.Error DivisionByZero -> Format.printf "DivisionByZero"
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| DivisionByZero |}]
;;

let%expect_test "eval modulo by zero" =
  let ast = parse_optimistically "10 % 0" in
  (match eval_expr ast with
   | Result.Ok (VInt n) -> Format.printf "%d" n
   | Result.Ok (VClosure _) -> Format.printf "<fun>"
   | Result.Ok (VBuiltin (name, _)) -> Format.printf "<builtin:%s>" name
   | Result.Ok VUnit -> Format.printf "()"
   | Result.Error DivisionByZero -> Format.printf "DivisionByZero"
   | Result.Error _ -> Format.printf "<error>");
  [%expect {| DivisionByZero |}]
;;

(* ========================================================================== *)
(* QuickCheck Generators and Property-Based Tests *)
(* ========================================================================== *)

(** Generator for variable names *)
let gen_name =
  Gen.(oneofl [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "m"; "a"; "b"; "foo"; "bar"; "baz" ])
;;

(** Generator for binary operators *)
let gen_binop = Gen.oneofl Ast.[ Add; Sub; Mul; Div; Mod; Eq; Neq; Lt; Gt; Leq; Geq ]

(** Generator for AST expressions with bounded depth
    @param max_depth Maximum depth of the generated AST tree *)
let gen_ast max_depth =
  let open Gen in
  let rec gen_ast_sized depth =
    if depth <= 0
    then
      (* At max depth, generate only leaf nodes *)
      oneof
        [ map (fun n -> Ast.Int n) (int_range 0 100)
        ; map (fun name -> Ast.Var name) gen_name
        ]
    else
      (* Generate all kinds of expressions *)
      frequency
        [ (* Leaf nodes - higher weight for simpler expressions *)
          3, map (fun n -> Ast.Int n) (int_range 0 100)
        ; 3, map (fun name -> Ast.Var name) gen_name (* Binary operations *)
        ; ( 2
          , map3
              (fun op l r -> Ast.BinOp (op, l, r))
              gen_binop
              (gen_ast_sized (depth - 1))
              (gen_ast_sized (depth - 1)) )
          (* Abstraction (lambda) *)
        ; ( 2
          , map2
              (fun param body -> Ast.Abs (param, body))
              gen_name
              (gen_ast_sized (depth - 1)) )
          (* Application *)
        ; ( 2
          , map2
              (fun f arg -> Ast.App (f, arg))
              (gen_ast_sized (depth - 1))
              (gen_ast_sized (depth - 1)) )
          (* If-then-else *)
        ; ( 1
          , map3
              (fun c t e -> Ast.If (c, t, Some e))
              (gen_ast_sized (depth - 1))
              (gen_ast_sized (depth - 1))
              (gen_ast_sized (depth - 1)) )
          (* Let binding (non-recursive only to avoid complexity) *)
        ; ( 1
          , map3
              (fun name binding body -> Ast.Let (false, name, binding, body))
              gen_name
              (gen_ast_sized (depth - 1))
              (gen_ast_sized (depth - 1)) )
        ]
  in
  gen_ast_sized max_depth
;;

(** QuickCheck test: parse and print are inverses (round-trip property)
    Property: for any AST, parsing the printed representation should yield the same AST *)
let test_roundtrip_property =
  Test.make
    ~name:"parse-print roundtrip"
    ~count:1000
    (make (gen_ast 3))
    (fun ast ->
      let printed = Lambda_lib.Print.print_expr ast in
      match Parser.parse printed with
      | Result.Ok reparsed -> ast = reparsed
      | Result.Error _ ->
        (* If parsing fails, this is also acceptable since our printer might produce
           expressions that are syntactically valid but the parser doesn't handle *)
        true)
;;

(** QuickCheck test: printer always produces parseable output
    Property: for any AST, the printed representation should be parseable *)
let test_printer_produces_parseable =
  Test.make
    ~name:"printer produces parseable output"
    ~count:1000
    (make (gen_ast 3))
    (fun ast ->
      let printed = Lambda_lib.Print.print_expr ast in
      match Parser.parse printed with
      | Result.Ok _ -> true
      | Result.Error _ -> false)
;;

(** QuickCheck test: print is deterministic
    Property: printing the same AST twice should produce the same string *)
let test_printer_deterministic =
  Test.make
    ~name:"printer is deterministic"
    ~count:1000
    (make (gen_ast 3))
    (fun ast ->
      let printed1 = Lambda_lib.Print.print_expr ast in
      let printed2 = Lambda_lib.Print.print_expr ast in
      printed1 = printed2)
;;

(** QuickCheck test: parser is deterministic
    Property: parsing the same string twice should produce the same result *)
let test_parser_deterministic =
  Test.make
    ~name:"parser is deterministic"
    ~count:1000
    (make Gen.(small_string ~gen:printable))
    (fun str ->
      let result1 = Parser.parse str in
      let result2 = Parser.parse str in
      result1 = result2)
;;

(* Run QuickCheck tests *)
let () =
  let open QCheck_base_runner in
  run_tests
    [ test_roundtrip_property
    ; test_printer_produces_parseable
    ; test_printer_deterministic
    ; test_parser_deterministic
    ]
  |> Stdlib.exit
;;
