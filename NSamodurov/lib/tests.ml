[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
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
  Format.printf "%a" pp (parse_optimistically "(\\x . x x)");
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Invalid_argument "result is Error _")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Tests.(fun) in file "lib/tests.ml", line 37, characters 24-60
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(λf.λx. f (x x))");
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Invalid_argument "result is Error _")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Tests.(fun) in file "lib/tests.ml", line 42, characters 24-67
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let _ = Lambda_lib.Interpret.parse_and_run
let _ = Lambda_lib.Lambda.a
let _ = Lambda_lib.Lambda.one
let _ = Lambda_lib.Lambda.p
let _ = Lambda_lib.Lambda.three
let _ = Lambda_lib.Lambda.two
let _ = Lambda_lib.Lambda.without_strat
let _ = Lambda_lib.Lambda.zero
let _ = Lambda_lib.Parser.parse_lam
let _ = Lambda_lib.Printast.pp
let _ = Lambda_lib.Printast.show
