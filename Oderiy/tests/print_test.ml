[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/"]

open Mml.Ast
open Mml.Print

(* Test helper *)
let show ast = print_string (to_string ast)

(* Literals *)
let%expect_test "printer test 1" =
  show (Int 987);
  [%expect {| 987 |}]
;;

let%expect_test "printer test 2" =
  show (Int 0);
  [%expect {| 0 |}]
;;

let%expect_test "printer test 3" =
  show (Neg (Int 123));
  [%expect {| (-123) |}]
;;

(* Variables *)
let%expect_test "printer test 4" =
  show (Var "myvar");
  [%expect {| myvar |}]
;;

let%expect_test "printer test 5" =
  show (Var "_secret");
  [%expect {| _secret |}]
;;

(* Arithmetic operations *)
let%expect_test "print addition" =
  show (Bin (Add, Int 7, Int 9));
  [%expect {| (7 + 9) |}]
;;

let%expect_test "print subtraction" =
  show (Bin (Sub, Int 25, Int 13));
  [%expect {| (25 - 13) |}]
;;

let%expect_test "print multiplication" =
  show (Bin (Mul, Int 8, Int 6));
  [%expect {| (8 * 6) |}]
;;

let%expect_test "print division" =
  show (Bin (Div, Int 48, Int 6));
  [%expect {| (48 / 6) |}]
;;

let%expect_test "print addition" =
  show (Bin (Add, Int 5, Bin (Mul, Int 7, Int 3)));
  [%expect {| (5 + (7 * 3)) |}]
;;

(* Comparisons *)
let%expect_test "print less than" =
  show (Bin (Lt, Int 11, Int 22));
  [%expect {| (11 < 22) |}]
;;

let%expect_test "print less or equal" =
  show (Bin (Leq, Int 33, Int 33));
  [%expect {| (33 <= 33) |}]
;;

let%expect_test "print equality" =
  show (Bin (Eq, Int 44, Int 44));
  [%expect {| (44 = 44) |}]
;;

let%expect_test "print greater or equal" =
  show (Bin (Geq, Int 55, Int 44));
  [%expect {| (55 >= 44) |}]
;;

let%expect_test "print greater than" =
  show (Bin (Gt, Int 66, Int 55));
  [%expect {| (66 > 55) |}]
;;

(* Lambdas *)
let%expect_test "print addition" =
  show (Fun ("arg", Bin (Add, Var "arg", Int 5)));
  [%expect {| (fun arg -> (arg + 5)) |}]
;;

let%expect_test "print multiplication" =
  show (Fun ("p", Fun ("q", Bin (Mul, Var "p", Var "q"))));
  [%expect {| (fun p -> (fun q -> (p * q))) |}]
;;

(* Applications *)
let%expect_test "print application" =
  show (App (Var "func", Var "value"));
  [%expect {| (func value) |}]
;;

let%expect_test "print application" =
  show (App (App (Var "g", Var "m"), Var "n"));
  [%expect {| ((g m) n) |}]
;;

let%expect_test "print addition" =
  show (App (Fun ("z", Bin (Add, Var "z", Int 7)), Int 15));
  [%expect {| ((fun z -> (z + 7)) 15) |}]
;;

(* Let expressions *)
let%expect_test "print integer" =
  show (Let ("num", Int 42, Bin (Add, Var "num", Int 8)));
  [%expect {| (let num = 42 in (num + 8)) |}]
;;

let%expect_test "print multiplication" =
  show
    (Let ("triple", Fun ("val", Bin (Mul, Var "val", Int 3)), App (Var "triple", Int 11)));
  [%expect {| (let triple = (fun val -> (val * 3)) in (triple 11)) |}]
;;

let%expect_test "print subtraction" =
  show (Let ("a", Int 10, Let ("b", Int 20, Bin (Sub, Var "b", Var "a"))));
  [%expect {| (let a = 10 in (let b = 20 in (b - a))) |}]
;;

(* Recursive let *)
let%expect_test "print subtraction" =
  let body =
    If
      ( Bin (Leq, Var "i", Int 1)
      , Int 1
      , Bin (Mul, Var "i", App (Var "fact", Bin (Sub, Var "i", Int 1))) )
  in
  show (LetRec ("fact", Fun ("i", body), App (Var "fact", Int 7)));
  [%expect
    {| (let rec fact = (fun i -> (if (i <= 1) then 1 else (i * (fact (i - 1))))) in (fact 7)) |}]
;;

(* Conditionals *)
let%expect_test "print greater than" =
  show (If (Bin (Gt, Var "m", Int 0), Int 77, Int 88));
  [%expect {| (if (m > 0) then 77 else 88) |}]
;;

let%expect_test "print greater than" =
  show
    (If (Bin (Gt, Var "p", Int 0), If (Bin (Gt, Var "p", Int 50), Int 2, Int 1), Int 0));
  [%expect {| (if (p > 0) then (if (p > 50) then 2 else 1) else 0) |}]
;;

(* Fix *)
let%expect_test "print fix operator" =
  show Fix;
  [%expect {| fix |}]
;;

(* Complex nested structures *)
let%expect_test "print addition" =
  let complex =
    LetRec
      ( "compute"
      , Fun
          ( "val"
          , If
              ( Bin (Leq, Var "val", Int 1)
              , Var "val"
              , Bin
                  ( Add
                  , App (Var "compute", Bin (Sub, Var "val", Int 1))
                  , App (Var "compute", Bin (Sub, Var "val", Int 2)) ) ) )
      , App (Var "compute", Int 9) )
  in
  show complex;
  [%expect
    {| (let rec compute = (fun val -> (if (val <= 1) then val else ((compute (val - 1)) + (compute (val - 2))))) in (compute 9)) |}]
;;

let%expect_test "print variable" =
  show
    (Let
       ( "x"
       , Int 4
       , Let
           ("y", Int 5, Let ("z", Int 6, Bin (Add, Var "x", Bin (Mul, Var "y", Var "z"))))
       ));
  [%expect {| (let x = 4 in (let y = 5 in (let z = 6 in (x + (y * z))))) |}]
;;
