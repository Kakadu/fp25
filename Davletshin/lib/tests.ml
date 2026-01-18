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

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "x y");
  [%expect {| (App ((Var x), (Var y))) |}]
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

let _ = Miniml_lib.Interpret.parse_and_run
let _ = Miniml_lib.Lambda.a
let _ = Miniml_lib.Lambda.one
let _ = Miniml_lib.Lambda.p
let _ = Miniml_lib.Lambda.three
let _ = Miniml_lib.Lambda.two
let _ = Miniml_lib.Lambda.zero
let _ = Miniml_lib.Parser.parse_lam
let _ = Miniml_lib.Printast.pp
let _ = Miniml_lib.Printast.show
