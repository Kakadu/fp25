[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Print AST in structural form for debugging
    Example: [Int 42], [Var "x"], [BinOp (Add, Int 1, Int 2)] *)
val print_ast : string Ast.t -> string

(** Print AST in human-readable form
    Example: [42], [x], [(1 + 2)] *)
val print_expr : string Ast.t -> string
