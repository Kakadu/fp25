[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let%test "parse constant" = test_parse "42" (Ast.Const 42)
let%test "parse negative constant" = test_parse "-5" (Ast.Const (-5))
let%test "parse variable" = test_parse "x" (Ast.Var "x")

let%test "parse addition" =
  test_parse "1 + 2" (Ast.BinOp (Ast.Add, Ast.Const 1, Ast.Const 2))
;;

let%test "parse subtraction" =
  test_parse "5 - 3" (Ast.BinOp (Ast.Sub, Ast.Const 5, Ast.Const 3))
;;

let%test "parse multiplication" =
  test_parse "4 * 5" (Ast.BinOp (Ast.Mul, Ast.Const 4, Ast.Const 5))
;;

let%test "parse division" =
  test_parse "10 / 2" (Ast.BinOp (Ast.Div, Ast.Const 10, Ast.Const 2))
;;

let%test "parse equality" =
  test_parse "5 = 5" (Ast.BinOp (Ast.Eq, Ast.Const 5, Ast.Const 5))
;;

let%test "parse less than" =
  test_parse "3 < 5" (Ast.BinOp (Ast.Lt, Ast.Const 3, Ast.Const 5))
;;

let%test "parse greater than" =
  test_parse "5 > 3" (Ast.BinOp (Ast.Gt, Ast.Const 5, Ast.Const 3))
;;

let%test "parse less or equal" =
  test_parse "5 <= 5" (Ast.BinOp (Ast.Le, Ast.Const 5, Ast.Const 5))
;;

let%test "parse greater or equal" =
  test_parse "5 >= 3" (Ast.BinOp (Ast.Ge, Ast.Const 5, Ast.Const 3))
;;

let%test "parse lambda" = test_parse "fun x -> x" (Ast.Abs ("x", Ast.Var "x"))

let%test "parse multi-arg lambda" =
  test_parse "fun x y -> x" (Ast.Abs ("x", Ast.Abs ("y", Ast.Var "x")))
;;

let%test "parse application" = test_parse "f x" (Ast.App (Ast.Var "f", Ast.Var "x"))

let%test "parse nested application" =
  test_parse "f x y" (Ast.App (Ast.App (Ast.Var "f", Ast.Var "x"), Ast.Var "y"))
;;

let%test "parse if-then-else" =
  test_parse "if x then 1 else 2" (Ast.If (Ast.Var "x", Ast.Const 1, Ast.Const 2))
;;

let%test "parse let binding" =
  test_parse "let x = 5 in x" (Ast.Let ("x", Ast.Const 5, Ast.Var "x"))
;;

let%test "parse let rec" =
  test_parse "let rec f x = x in f" (Ast.LetRec ("f", "x", Ast.Var "x", Ast.Var "f"))
;;

let%test "parse parenthesized expression" = test_parse "(5)" (Ast.Const 5)

let%test "parse operator precedence mul before add" =
  test_parse
    "1 + 2 * 3"
    (Ast.BinOp (Ast.Add, Ast.Const 1, Ast.BinOp (Ast.Mul, Ast.Const 2, Ast.Const 3)))
;;

let%test "parse operator precedence with parentheses" =
  test_parse
    "(1 + 2) * 3"
    (Ast.BinOp (Ast.Mul, Ast.BinOp (Ast.Add, Ast.Const 1, Ast.Const 2), Ast.Const 3))
;;

let%test "parse complex let" =
  test_parse
    "let x = 5 in let y = 3 in x + y"
    (Ast.Let
       ( "x"
       , Ast.Const 5
       , Ast.Let ("y", Ast.Const 3, Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y")) ))
;;

let%test "parse function definition sugar" =
  test_parse "let f x = x in f" (Ast.Let ("f", Ast.Abs ("x", Ast.Var "x"), Ast.Var "f"))
;;

let%test "parse multi-arg function definition" =
  test_parse
    "let add x y = x + y in add"
    (Ast.Let
       ( "add"
       , Ast.Abs ("x", Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y")))
       , Ast.Var "add" ))
;;

let%test "parse println_int" =
  test_parse "println_int 42" (Ast.App (Ast.Var "println_int", Ast.Const 42))
;;

let%test "parse parenthesized println_int" =
  test_parse "(println_int 42)" (Ast.App (Ast.Var "println_int", Ast.Const 42))
;;

let%test "parse empty string fails" = test_parse_error ""
let%test "parse invalid syntax fails" = test_parse_error "let x ="

let%test "parse application associativity" =
  test_parse "a b c" (Ast.App (Ast.App (Ast.Var "a", Ast.Var "b"), Ast.Var "c"))
;;

let%test "parse comparison in condition" =
  test_parse
    "if x < 5 then 1 else 0"
    (Ast.If (Ast.BinOp (Ast.Lt, Ast.Var "x", Ast.Const 5), Ast.Const 1, Ast.Const 0))
;;

let%test "parse nested let bindings" =
  test_parse
    "let a = 1 in let b = 2 in let c = 3 in a + b + c"
    (Ast.Let
       ( "a"
       , Ast.Const 1
       , Ast.Let
           ( "b"
           , Ast.Const 2
           , Ast.Let
               ( "c"
               , Ast.Const 3
               , Ast.BinOp
                   (Ast.Add, Ast.BinOp (Ast.Add, Ast.Var "a", Ast.Var "b"), Ast.Var "c")
               ) ) ))
;;

let%test "parse complex arithmetic" =
  test_parse
    "10 - 5 * 2 + 3 / 1"
    (Ast.BinOp
       ( Ast.Add
       , Ast.BinOp (Ast.Sub, Ast.Const 10, Ast.BinOp (Ast.Mul, Ast.Const 5, Ast.Const 2))
       , Ast.BinOp (Ast.Div, Ast.Const 3, Ast.Const 1) ))
;;

let%test "parse nested if expressions" =
  test_parse
    "if x then if y then 1 else 2 else 3"
    (Ast.If (Ast.Var "x", Ast.If (Ast.Var "y", Ast.Const 1, Ast.Const 2), Ast.Const 3))
;;

let%test "parse let rec with multiple args" =
  test_parse
    "let rec add x y = x + y in add"
    (Ast.LetRec
       ( "add"
       , "x"
       , Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y"))
       , Ast.Var "add" ))
;;

let%test "parse let rec factorial" =
  test_parse
    "let rec fact n = if n > 0 then n * fact (n - 1) else 1 in fact 5"
    (Ast.LetRec
       ( "fact"
       , "n"
       , Ast.If
           ( Ast.BinOp (Ast.Gt, Ast.Var "n", Ast.Const 0)
           , Ast.BinOp
               ( Ast.Mul
               , Ast.Var "n"
               , Ast.App (Ast.Var "fact", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1))
               )
           , Ast.Const 1 )
       , Ast.App (Ast.Var "fact", Ast.Const 5) ))
;;

let%test "parse application with operators" =
  test_parse
    "f (x + 1)"
    (Ast.App (Ast.Var "f", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1)))
;;

let%test "parse triple application" =
  test_parse
    "a b c d"
    (Ast.App (Ast.App (Ast.App (Ast.Var "a", Ast.Var "b"), Ast.Var "c"), Ast.Var "d"))
;;

let%test "parse lambda with body binop" =
  test_parse
    "fun x -> x + 1"
    (Ast.Abs ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1)))
;;

let%test "parse lambda with if body" =
  test_parse
    "fun x -> if x then 1 else 0"
    (Ast.Abs ("x", Ast.If (Ast.Var "x", Ast.Const 1, Ast.Const 0)))
;;

let%test "parse three-arg lambda" =
  test_parse "fun x y z -> x" (Ast.Abs ("x", Ast.Abs ("y", Ast.Abs ("z", Ast.Var "x"))))
;;

let%test "parse let in lambda" =
  test_parse
    "fun x -> let y = x in y"
    (Ast.Abs ("x", Ast.Let ("y", Ast.Var "x", Ast.Var "y")))
;;

let%test "parse lambda in let value" =
  test_parse
    "let f = fun x -> x in f"
    (Ast.Let ("f", Ast.Abs ("x", Ast.Var "x"), Ast.Var "f"))
;;

let%test "parse comparison chain" =
  test_parse
    "x < y <= z"
    (Ast.BinOp (Ast.Le, Ast.BinOp (Ast.Lt, Ast.Var "x", Ast.Var "y"), Ast.Var "z"))
;;

let%test "parse all comparison operators" =
  test_parse "a = b" (Ast.BinOp (Ast.Eq, Ast.Var "a", Ast.Var "b"))
  && test_parse "a < b" (Ast.BinOp (Ast.Lt, Ast.Var "a", Ast.Var "b"))
  && test_parse "a > b" (Ast.BinOp (Ast.Gt, Ast.Var "a", Ast.Var "b"))
  && test_parse "a <= b" (Ast.BinOp (Ast.Le, Ast.Var "a", Ast.Var "b"))
  && test_parse "a >= b" (Ast.BinOp (Ast.Ge, Ast.Var "a", Ast.Var "b"))
;;

let%test "parse all arithmetic operators" =
  test_parse "a + b" (Ast.BinOp (Ast.Add, Ast.Var "a", Ast.Var "b"))
  && test_parse "a - b" (Ast.BinOp (Ast.Sub, Ast.Var "a", Ast.Var "b"))
  && test_parse "a * b" (Ast.BinOp (Ast.Mul, Ast.Var "a", Ast.Var "b"))
  && test_parse "a / b" (Ast.BinOp (Ast.Div, Ast.Var "a", Ast.Var "b"))
;;

let%test "parse deeply nested parentheses" = test_parse "(((((5)))))" (Ast.Const 5)

let%test "parse complex function application" =
  test_parse
    "let f x y = x + y in f 3 5"
    (Ast.Let
       ( "f"
       , Ast.Abs ("x", Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y")))
       , Ast.App (Ast.App (Ast.Var "f", Ast.Const 3), Ast.Const 5) ))
;;

let%test "parse mixed operators precedence 1" =
  test_parse
    "a + b * c"
    (Ast.BinOp (Ast.Add, Ast.Var "a", Ast.BinOp (Ast.Mul, Ast.Var "b", Ast.Var "c")))
;;

let%test "parse mixed operators precedence 2" =
  test_parse
    "a * b + c"
    (Ast.BinOp (Ast.Add, Ast.BinOp (Ast.Mul, Ast.Var "a", Ast.Var "b"), Ast.Var "c"))
;;

let%test "parse mixed operators precedence 3" =
  test_parse
    "a - b / c"
    (Ast.BinOp (Ast.Sub, Ast.Var "a", Ast.BinOp (Ast.Div, Ast.Var "b", Ast.Var "c")))
;;

let%test "parse parentheses override precedence" =
  test_parse
    "(a + b) * c"
    (Ast.BinOp (Ast.Mul, Ast.BinOp (Ast.Add, Ast.Var "a", Ast.Var "b"), Ast.Var "c"))
;;

let%test "parse if in application" =
  test_parse
    "f (if x then 1 else 0)"
    (Ast.App (Ast.Var "f", Ast.If (Ast.Var "x", Ast.Const 1, Ast.Const 0)))
;;

let%test "parse let rec with if" =
  test_parse
    "let rec even n = if n = 0 then 1 else 0 in even"
    (Ast.LetRec
       ( "even"
       , "n"
       , Ast.If (Ast.BinOp (Ast.Eq, Ast.Var "n", Ast.Const 0), Ast.Const 1, Ast.Const 0)
       , Ast.Var "even" ))
;;

let%test "parse zero" = test_parse "0" (Ast.Const 0)
let%test "parse large number" = test_parse "12345" (Ast.Const 12345)
let%test "parse negative number" = test_parse "-999" (Ast.Const (-999))

let%test "parse single char variables" =
  test_parse "a" (Ast.Var "a")
  && test_parse "x" (Ast.Var "x")
  && test_parse "z" (Ast.Var "z")
;;

let%test "parse multi char variable" = test_parse "abc123" (Ast.Var "abc123")

let%test "parse let with same var names" =
  test_parse
    "let x = 1 in let x = 2 in x"
    (Ast.Let ("x", Ast.Const 1, Ast.Let ("x", Ast.Const 2, Ast.Var "x")))
;;

let%test "parse curried function call" =
  test_parse
    "f x y z"
    (Ast.App (Ast.App (Ast.App (Ast.Var "f", Ast.Var "x"), Ast.Var "y"), Ast.Var "z"))
;;

let%test "parse operator with parentheses" =
  test_parse "(1 + 2)" (Ast.BinOp (Ast.Add, Ast.Const 1, Ast.Const 2))
;;

let%test "parse multiple parentheses levels" =
  test_parse
    "((1 + 2) * (3 + 4))"
    (Ast.BinOp
       ( Ast.Mul
       , Ast.BinOp (Ast.Add, Ast.Const 1, Ast.Const 2)
       , Ast.BinOp (Ast.Add, Ast.Const 3, Ast.Const 4) ))
;;

let%test "parse lambda application chain" =
  test_parse
    "(fun x -> x) (fun y -> y)"
    (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Abs ("y", Ast.Var "y")))
;;

let%test "parse let with complex body" =
  test_parse
    "let f = fun x -> x + 1 in f 10"
    (Ast.Let
       ( "f"
       , Ast.Abs ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1))
       , Ast.App (Ast.Var "f", Ast.Const 10) ))
;;

let%test "parse invalid: unclosed paren" = test_parse_error "(1 + 2"
let%test "parse invalid: extra closing paren" = test_parse_error "1 + 2)"
let%test "parse invalid: missing then" = test_parse_error "if x else 0"
let%test "parse invalid: missing else" = test_parse_error "if x then 1"
let%test "parse invalid: incomplete let" = test_parse_error "let x = 5"
let%test "parse invalid: missing in" = test_parse_error "let x = 5 x"
let%test "parse invalid: operator without operands" = test_parse_error "+"
let%test "parse invalid: double operator" = test_parse_error "1 + + 2"
