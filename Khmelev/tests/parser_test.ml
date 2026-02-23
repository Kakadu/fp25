[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let test_parse input expected =
  match Parser.parse input with
  | Ok ast when ast = expected -> true
  | _ -> false
;;

let test_parse_error input =
  match Parser.parse input with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse constant" = test_parse "42" (Const 42)
let%test "parse negative constant" = test_parse "-5" (Const (-5))
let%test "parse variable" = test_parse "x" (Var "x")
let%test "parse addition" = test_parse "1 + 2" (BinOp (Add, Const 1, Const 2))
let%test "parse subtraction" = test_parse "5 - 3" (BinOp (Sub, Const 5, Const 3))
let%test "parse multiplication" = test_parse "4 * 5" (BinOp (Mul, Const 4, Const 5))
let%test "parse division" = test_parse "10 / 2" (BinOp (Div, Const 10, Const 2))
let%test "parse equality" = test_parse "5 = 5" (BinOp (Eq, Const 5, Const 5))
let%test "parse less than" = test_parse "3 < 5" (BinOp (Lt, Const 3, Const 5))
let%test "parse greater than" = test_parse "5 > 3" (BinOp (Gt, Const 5, Const 3))
let%test "parse less or equal" = test_parse "5 <= 5" (BinOp (Le, Const 5, Const 5))
let%test "parse greater or equal" = test_parse "5 >= 3" (BinOp (Ge, Const 5, Const 3))
let%test "parse lambda" = test_parse "fun x -> x" (Abs ("x", Var "x"))

let%test "parse multi-arg lambda" =
  test_parse "fun x y -> x" (Abs ("x", Abs ("y", Var "x")))
;;

let%test "parse application" = test_parse "f x" (App (Var "f", Var "x"))

let%test "parse nested application" =
  test_parse "f x y" (App (App (Var "f", Var "x"), Var "y"))
;;

let%test "parse if-then-else" =
  test_parse "if x then 1 else 2" (If (Var "x", Const 1, Const 2))
;;

let%test "parse let binding" = test_parse "let x = 5 in x" (Let ("x", Const 5, Var "x"))

let%test "parse let rec" =
  test_parse "let rec f x = x in f" (LetRec ("f", "x", Var "x", Var "f"))
;;

let%test "parse parenthesized expression" = test_parse "(5)" (Const 5)

let%test "parse operator precedence mul before add" =
  test_parse "1 + 2 * 3" (BinOp (Add, Const 1, BinOp (Mul, Const 2, Const 3)))
;;

let%test "parse operator precedence with parentheses" =
  test_parse "(1 + 2) * 3" (BinOp (Mul, BinOp (Add, Const 1, Const 2), Const 3))
;;

let%test "parse complex let" =
  test_parse
    "let x = 5 in let y = 3 in x + y"
    (Let ("x", Const 5, Let ("y", Const 3, BinOp (Add, Var "x", Var "y"))))
;;

let%test "parse function definition sugar" =
  test_parse "let f x = x in f" (Let ("f", Abs ("x", Var "x"), Var "f"))
;;

let%test "parse multi-arg function definition" =
  test_parse
    "let add x y = x + y in add"
    (Let ("add", Abs ("x", Abs ("y", BinOp (Add, Var "x", Var "y"))), Var "add"))
;;

let%test "parse println_int" =
  test_parse "println_int 42" (App (Var "println_int", Const 42))
;;

let%test "parse parenthesized println_int" =
  test_parse "(println_int 42)" (App (Var "println_int", Const 42))
;;

let%test "parse empty string fails" = test_parse_error ""
let%test "parse invalid syntax fails" = test_parse_error "let x ="

let%test "parse application associativity" =
  test_parse "a b c" (App (App (Var "a", Var "b"), Var "c"))
;;

let%test "parse comparison in condition" =
  test_parse
    "if x < 5 then 1 else 0"
    (If (BinOp (Lt, Var "x", Const 5), Const 1, Const 0))
;;

let%test "parse nested let bindings" =
  test_parse
    "let a = 1 in let b = 2 in let c = 3 in a + b + c"
    (Let
       ( "a"
       , Const 1
       , Let
           ( "b"
           , Const 2
           , Let ("c", Const 3, BinOp (Add, BinOp (Add, Var "a", Var "b"), Var "c")) ) ))
;;

let%test "parse complex arithmetic" =
  test_parse
    "10 - 5 * 2 + 3 / 1"
    (BinOp
       ( Add
       , BinOp (Sub, Const 10, BinOp (Mul, Const 5, Const 2))
       , BinOp (Div, Const 3, Const 1) ))
;;

let%test "parse nested if expressions" =
  test_parse
    "if x then if y then 1 else 2 else 3"
    (If (Var "x", If (Var "y", Const 1, Const 2), Const 3))
;;

let%test "parse let rec with multiple args" =
  test_parse
    "let rec add x y = x + y in add"
    (LetRec ("add", "x", Abs ("y", BinOp (Add, Var "x", Var "y")), Var "add"))
;;

let%test "parse let rec factorial" =
  test_parse
    "let rec fact n = if n > 0 then n * fact (n - 1) else 1 in fact 5"
    (LetRec
       ( "fact"
       , "n"
       , If
           ( BinOp (Gt, Var "n", Const 0)
           , BinOp (Mul, Var "n", App (Var "fact", BinOp (Sub, Var "n", Const 1)))
           , Const 1 )
       , App (Var "fact", Const 5) ))
;;

let%test "parse application with operators" =
  test_parse "f (x + 1)" (App (Var "f", BinOp (Add, Var "x", Const 1)))
;;

let%test "parse triple application" =
  test_parse "a b c d" (App (App (App (Var "a", Var "b"), Var "c"), Var "d"))
;;

let%test "parse lambda with body binop" =
  test_parse "fun x -> x + 1" (Abs ("x", BinOp (Add, Var "x", Const 1)))
;;

let%test "parse lambda with if body" =
  test_parse "fun x -> if x then 1 else 0" (Abs ("x", If (Var "x", Const 1, Const 0)))
;;

let%test "parse three-arg lambda" =
  test_parse "fun x y z -> x" (Abs ("x", Abs ("y", Abs ("z", Var "x"))))
;;

let%test "parse let in lambda" =
  test_parse "fun x -> let y = x in y" (Abs ("x", Let ("y", Var "x", Var "y")))
;;

let%test "parse lambda in let value" =
  test_parse "let f = fun x -> x in f" (Let ("f", Abs ("x", Var "x"), Var "f"))
;;

let%test "parse comparison chain" =
  test_parse "x < y <= z" (BinOp (Le, BinOp (Lt, Var "x", Var "y"), Var "z"))
;;

let%test "parse all comparison operators" =
  test_parse "a = b" (BinOp (Eq, Var "a", Var "b"))
  && test_parse "a < b" (BinOp (Lt, Var "a", Var "b"))
  && test_parse "a > b" (BinOp (Gt, Var "a", Var "b"))
  && test_parse "a <= b" (BinOp (Le, Var "a", Var "b"))
  && test_parse "a >= b" (BinOp (Ge, Var "a", Var "b"))
;;

let%test "parse all arithmetic operators" =
  test_parse "a + b" (BinOp (Add, Var "a", Var "b"))
  && test_parse "a - b" (BinOp (Sub, Var "a", Var "b"))
  && test_parse "a * b" (BinOp (Mul, Var "a", Var "b"))
  && test_parse "a / b" (BinOp (Div, Var "a", Var "b"))
;;

let%test "parse deeply nested parentheses" = test_parse "(((((5)))))" (Const 5)

let%test "parse complex function application" =
  test_parse
    "let f x y = x + y in f 3 5"
    (Let
       ( "f"
       , Abs ("x", Abs ("y", BinOp (Add, Var "x", Var "y")))
       , App (App (Var "f", Const 3), Const 5) ))
;;

let%test "parse mixed operators precedence 1" =
  test_parse "a + b * c" (BinOp (Add, Var "a", BinOp (Mul, Var "b", Var "c")))
;;

let%test "parse mixed operators precedence 2" =
  test_parse "a * b + c" (BinOp (Add, BinOp (Mul, Var "a", Var "b"), Var "c"))
;;

let%test "parse mixed operators precedence 3" =
  test_parse "a - b / c" (BinOp (Sub, Var "a", BinOp (Div, Var "b", Var "c")))
;;

let%test "parse parentheses override precedence" =
  test_parse "(a + b) * c" (BinOp (Mul, BinOp (Add, Var "a", Var "b"), Var "c"))
;;

let%test "parse if in application" =
  test_parse "f (if x then 1 else 0)" (App (Var "f", If (Var "x", Const 1, Const 0)))
;;

let%test "parse let rec with if" =
  test_parse
    "let rec even n = if n = 0 then 1 else 0 in even"
    (LetRec ("even", "n", If (BinOp (Eq, Var "n", Const 0), Const 1, Const 0), Var "even"))
;;

let%test "parse zero" = test_parse "0" (Const 0)
let%test "parse large number" = test_parse "12345" (Const 12345)
let%test "parse negative number" = test_parse "-999" (Const (-999))

let%test "parse single char variables" =
  test_parse "a" (Var "a") && test_parse "x" (Var "x") && test_parse "z" (Var "z")
;;

let%test "parse multi char variable" = test_parse "abc123" (Var "abc123")

let%test "parse let with same var names" =
  test_parse
    "let x = 1 in let x = 2 in x"
    (Let ("x", Const 1, Let ("x", Const 2, Var "x")))
;;

let%test "parse curried function call" =
  test_parse "f x y z" (App (App (App (Var "f", Var "x"), Var "y"), Var "z"))
;;

let%test "parse operator with parentheses" =
  test_parse "(1 + 2)" (BinOp (Add, Const 1, Const 2))
;;

let%test "parse multiple parentheses levels" =
  test_parse
    "((1 + 2) * (3 + 4))"
    (BinOp (Mul, BinOp (Add, Const 1, Const 2), BinOp (Add, Const 3, Const 4)))
;;

let%test "parse lambda application chain" =
  test_parse "(fun x -> x) (fun y -> y)" (App (Abs ("x", Var "x"), Abs ("y", Var "y")))
;;

let%test "parse let with complex body" =
  test_parse
    "let f = fun x -> x + 1 in f 10"
    (Let ("f", Abs ("x", BinOp (Add, Var "x", Const 1)), App (Var "f", Const 10)))
;;

let%test "parse invalid: unclosed paren" = test_parse_error "(1 + 2"
let%test "parse invalid: extra closing paren" = test_parse_error "1 + 2)"
let%test "parse invalid: missing then" = test_parse_error "if x else 0"
let%test "parse invalid: missing else" = test_parse_error "if x then 1"
let%test "parse invalid: incomplete let" = test_parse_error "let x = 5"
let%test "parse invalid: missing in" = test_parse_error "let x = 5 x"
let%test "parse invalid: operator without operands" = test_parse_error "+"
let%test "parse invalid: double operator" = test_parse_error "1 + + 2"
