(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Ast
open Filichkin_lib.Print

let%expect_test "print_type_decl" =
  let td =
    { type_name = "color"
    ; type_params = []
    ; constructors =
        [ { ctor_name = "Red"; ctor_args = [] }
        ; { ctor_name = "RGB"; ctor_args = [ TEInt; TEInt; TEInt ] }
        ]
    }
  in
  print_string (toplevel_to_string (TLType td));
  [%expect {| type color = Red | RGB of int * int * int |}]
;;

let%expect_test "print_expr_fun_tuple" =
  let expr = Abs (PTuple [ PVar "x"; PVar "y" ], BinOp (Plus, Var "x", Var "y")) in
  print_string (print_expr expr);
  [%expect {| (fun (x, y) -> ((x) + (y))) |}]
;;

let%expect_test "print_expr_match" =
  let expr =
    Match
      ( Var "xs"
      ,
        [ PConstr ("Nil", []), Int 0
        ; PConstr ("Cons", [ PVar "x"; PVar "xs" ]), Var "x"
        ] )
  in
  print_string (print_expr expr);
  [%expect {| (match xs with | Nil -> 0 | Cons (x, xs) -> x) |}]
;;
