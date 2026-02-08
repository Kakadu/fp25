[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

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

open Miniml_lib
open Parser

let parse_optimistically str = Result.get_ok (parse str)
let pp = Printast.pp_named

(* ---------- old tests ---------- *)

let%expect_test _ =
  Format.printf
    "%a"
    pp
    (parse_optimistically
       "let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 5");
  [%expect
    {|
    (Let (Rec, fact,
       (Abs (n,
          (If ((Binop (Eq, (Var n), (Int 1))), (Int 1),
             (Binop (Times, (Var n),
                (App ((Var fact), (Binop (Minus, (Var n), (Int 1)))))))
             ))
          )),
       (App ((Var fact), (Int 5))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(x y)");
  [%expect {| (App ((Var x), (Var y))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun x -> x x)");
  [%expect {| (Abs (x, (App ((Var x), (Var x))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun f -> fun x -> f (x x))");
  [%expect {| (Abs (f, (Abs (x, (App ((Var f), (App ((Var x), (Var x))))))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun f -> fun x -> f (1 1))");
  [%expect {| (Abs (f, (Abs (x, (App ((Var f), (App ((Int 1), (Int 1))))))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun f -> 1 + 2)");
  [%expect {| (Abs (f, (Binop (Plus, (Int 1), (Int 2))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "1 + if 1 then 1 else 2");
  [%expect {| (Binop (Plus, (Int 1), (If ((Int 1), (Int 1), (Int 2))))) |}]
;;

(* ---------- new tests ---------- *)

(* ---------- literals & identifiers ---------- *)

let%expect_test "int literal" =
  Format.printf "%a" pp (parse_optimistically "42");
  [%expect {| (Int 42) |}]
;;

let%expect_test "identifier" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect {| (Var x) |}]
;;

(* ---------- parentheses ---------- *)

let%expect_test "parenthesized expression" =
  Format.printf "%a" pp (parse_optimistically "(42)");
  [%expect {| (Int 42) |}]
;;

let%expect_test "nested parentheses" =
  Format.printf "%a" pp (parse_optimistically "(((x)))");
  [%expect {| (Var x) |}]
;;

(* ---------- functions ---------- *)

let%expect_test "simple function" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x");
  [%expect {| (Abs (x, (Var x))) |}]
;;

let%expect_test "function body precedence" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x + 1");
  [%expect {| (Abs (x, (Binop (Plus, (Var x), (Int 1))))) |}]
;;

let%expect_test "nested function" =
  Format.printf "%a" pp (parse_optimistically "fun x -> fun y -> x");
  [%expect {| (Abs (x, (Abs (y, (Var x))))) |}]
;;

(* ---------- application ---------- *)

let%expect_test "simple application" =
  Format.printf "%a" pp (parse_optimistically "f x");
  [%expect {| (App ((Var f), (Var x))) |}]
;;

let%expect_test "left associative application" =
  Format.printf "%a" pp (parse_optimistically "f x y");
  [%expect {| (App ((App ((Var f), (Var x))), (Var y))) |}]
;;

let%expect_test "application vs operator precedence" =
  Format.printf "%a" pp (parse_optimistically "f x + y");
  [%expect {| (Binop (Plus, (App ((Var f), (Var x))), (Var y))) |}]
;;

let%expect_test "application with parentheses" =
  Format.printf "%a" pp (parse_optimistically "f (x + 1)");
  [%expect {| (App ((Var f), (Binop (Plus, (Var x), (Int 1))))) |}]
;;

(* ---------- unary minus ---------- *)

let%expect_test "unary minus literal" =
  Format.printf "%a" pp (parse_optimistically "-1");
  [%expect {| (Unop (Neg, (Int 1))) |}]
;;

let%expect_test "unary minus variable" =
  Format.printf "%a" pp (parse_optimistically "-x");
  [%expect {| (Unop (Neg, (Var x))) |}]
;;

let%expect_test "unary minus precedence" =
  Format.printf "%a" pp (parse_optimistically "-x * y");
  [%expect {| (Binop (Times, (Unop (Neg, (Var x))), (Var y))) |}]
;;

(* ---------- arithmetic precedence ---------- *)

let%expect_test "multiplication binds tighter" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 * 3");
  [%expect {| (Binop (Plus, (Int 1), (Binop (Times, (Int 2), (Int 3))))) |}]
;;

let%expect_test "left associative addition" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 + 3");
  [%expect {| (Binop (Plus, (Binop (Plus, (Int 1), (Int 2))), (Int 3))) |}]
;;

let%expect_test "parentheses override precedence" =
  Format.printf "%a" pp (parse_optimistically "(1 + 2) * 3");
  [%expect {| (Binop (Times, (Binop (Plus, (Int 1), (Int 2))), (Int 3))) |}]
;;

(* ---------- comparisons ---------- *)

let%expect_test "equality" =
  Format.printf "%a" pp (parse_optimistically "x = y");
  [%expect {| (Binop (Eq, (Var x), (Var y))) |}]
;;

let%expect_test "inequality" =
  Format.printf "%a" pp (parse_optimistically "x <> y");
  [%expect {| (Binop (Neq, (Var x), (Var y))) |}]
;;

let%expect_test "comparison precedence" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 = 3");
  [%expect {| (Binop (Eq, (Binop (Plus, (Int 1), (Int 2))), (Int 3))) |}]
;;

(* ---------- if expressions ---------- *)

let%expect_test "simple if" =
  Format.printf "%a" pp (parse_optimistically "if x then y else z");
  [%expect {| (If ((Var x), (Var y), (Var z))) |}]
;;

let%expect_test "if with operators" =
  Format.printf "%a" pp (parse_optimistically "if x = 0 then 1 else 2");
  [%expect {| (If ((Binop (Eq, (Var x), (Int 0))), (Int 1), (Int 2))) |}]
;;

let%expect_test "nested if" =
  Format.printf "%a" pp (parse_optimistically "if x then if y then a else b else c");
  [%expect {|
(If ((Var x), (If ((Var y), (Var a), (Var b))), (Var c)))
|}]
;;

(* ---------- let bindings ---------- *)

let%expect_test "simple let" =
  Format.printf "%a" pp (parse_optimistically "let x = 1 in x");
  [%expect {| (Let (Nonrec, x, (Int 1), (Var x))) |}]
;;

let%expect_test "let precedence" =
  Format.printf "%a" pp (parse_optimistically "let x = 1 in x + 2");
  [%expect {| (Let (Nonrec, x, (Int 1), (Binop (Plus, (Var x), (Int 2))))) |}]
;;

let%expect_test "nested let" =
  Format.printf "%a" pp (parse_optimistically "let x = 1 in let y = 2 in x + y");
  [%expect
    {|
(Let (Nonrec, x, (Int 1),
   (Let (Nonrec, y, (Int 2), (Binop (Plus, (Var x), (Var y)))))))
|}]
;;

(* ---------- let rec ---------- *)

let%expect_test "simple let rec" =
  Format.printf "%a" pp (parse_optimistically "let rec f = fun x -> x in f");
  [%expect {|
(Let (Rec, f, (Abs (x, (Var x))), (Var f)))
|}]
;;

let%expect_test "recursive application" =
  Format.printf "%a" pp (parse_optimistically "let rec f = fun x -> f x in f 1");
  [%expect
    {|
(Let (Rec, f, (Abs (x, (App ((Var f), (Var x))))), (App ((Var f), (Int 1)))))
|}]
;;

(* ---------- tricky precedence combos ---------- *)

let%expect_test "application inside let" =
  Format.printf "%a" pp (parse_optimistically "let f = fun x -> x in f 3");
  [%expect {|
(Let (Nonrec, f, (Abs (x, (Var x))), (App ((Var f), (Int 3)))))
|}]
;;

let%expect_test "if as function argument" =
  Format.printf "%a" pp (parse_optimistically "f (if x then y else z)");
  [%expect {|
(App ((Var f), (If ((Var x), (Var y), (Var z)))))
|}]
;;

let%expect_test "function returning function applied" =
  Format.printf "%a" pp (parse_optimistically "(fun x -> fun y -> x) a b");
  [%expect {|
(App ((App ((Abs (x, (Abs (y, (Var x))))), (Var a))), (Var b)))
|}]
;;

let _ = Miniml_lib.Interpret.parse_and_run
let _ = Miniml_lib.Parser.parser
let _ = Miniml_lib.Printast.pp
let _ = Miniml_lib.Printast.show
