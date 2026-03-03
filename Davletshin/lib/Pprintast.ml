[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast

let pp =
  let open Format in
  let rec pp fmt = function
    | Int n -> fprintf fmt "%d" n
    | Var s -> fprintf fmt "%s" s
    | App (l, r) -> fprintf fmt "(%a %a)" pp l pp r
    | Abs (x, t) -> fprintf fmt "(fun %s -> %a)" x pp t
    | Binop (op, l, r) ->
      let bop =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Divide -> "/"
        | Eq -> "="
        | Neq -> "<>"
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Ge -> ">="
      in
      fprintf fmt "(%a %s %a)" pp l bop pp r
    | Unop (op, e) ->
      let uop =
        match op with
        | Pos -> "+"
        | Neg -> "-"
      in
      fprintf fmt "(%s%a)" uop pp e
    | If (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp c pp t pp e
    | Let (Nonrec, n, e1, e2) -> fprintf fmt "(let %s = %a in %a)" n pp e1 pp e2
    | Let (Rec, n, e1, e2) -> fprintf fmt "(let rec %s = %a in %a)" n pp e1 pp e2
    | Fix f -> fprintf fmt "(fix %a)" pp f
    | Print e -> fprintf fmt "(print %a)" pp e
  in
  pp
;;

let pp_hum = pp
