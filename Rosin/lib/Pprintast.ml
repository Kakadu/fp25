[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast

let parens ppf f =
  fprintf ppf "(";
  f ppf ();
  fprintf ppf ")"
;;

let rec pp =
  let pp_unop = function
    | Inc -> "Inc"
    | Dec -> "Dec"
  in
  let pp_binop = function
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Mult -> "Mult"
    | Div -> "Div"
  in
  let pp fmt = function
    | Num n -> fprintf fmt "Int(%d)" n
    | Var v -> fprintf fmt "Var(%s)" v
    | Unop (op, a) -> parens fmt (fun ppf () -> fprintf ppf "%s(%a)" (pp_unop op) pp a)
    | Binop (op, a, b) ->
      parens fmt (fun ppf () -> fprintf ppf "%s(%a, %a)" (pp_binop op) pp a pp b)
    | If (cond, then_e, else_e) ->
      let res =
        match else_e with
        | Some else_e ->
          fprintf fmt "If(%a) Then(%a) Else (%a))" pp cond pp then_e pp else_e
        | None -> fprintf fmt "If(%a) Then(%a)" pp cond pp then_e
      in
      res
    | Fun (name, e) -> fprintf fmt "Fun(%s, %a)" name pp e
    | Let (name, rhs, body) -> fprintf fmt "Let(%s, %a) in %a" name pp rhs pp body
    | Letrec (name, rhs, body) -> fprintf fmt "Letrec((%s, %a) in %a)" name pp rhs pp body
    | Fix e -> fprintf fmt "Fix(%a)" pp e
    | App (f, x) -> parens fmt (fun ppf () -> fprintf ppf "App(%a, %a)" pp f pp x)
    | Print e -> fprintf fmt "Print(%a)" pp e
  in
  pp
;;
