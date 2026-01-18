(** Copyright 2026, Kirill K. Smirnov *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Mardukml_lib

let%test "lterm_to_string var" =
    Lambda.lterm_to_string (Var "x") =
    "x"

let%test "lterm_to_string app" =
    Lambda.lterm_to_string (App (Var "x", Var "y")) =
    "(x y)"

let%test "lterm_to_string abs" =
    Lambda.lterm_to_string (Abs ("x", Var "y")) =
    "(\\x.y)"

let%test "fvars 1" =
    Lambda.fvars (Var "x") =
    ["x"]

let%test "fvars 2" =
    Lambda.fvars (Abs ("x", Var "x")) =
    []

let%test "subst 1" =
    Lambda.subst (Var "x") "x" (App (Var "x", Var "y")) =
    App (Var "x", Var "y")

let%test "subst 2" =
    Lambda.subst (Var "x") "y" (App (Var "x", Var "y")) =
    Var "x"

let%test "subst 3" =
    Lambda.subst (Abs ("x", App (Var "x", Var "y"))) "y" (Var "x") =
    Abs ("fv0", App (Var "fv0", Var "x"))

let%test "compile 1" =
    Mardukml.compile_to_lambda_cbv (Var "x") =
    (Var "x")

let%test "compile 2" =
    Mardukml.compile_to_lambda_cbv (Fun ("x", Var "y")) =
    Abs("x", Var "y")
