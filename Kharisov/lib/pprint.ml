[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mul -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Eq -> Format.fprintf fmt "="
  | Neq -> Format.fprintf fmt "<>"
  | Lt -> Format.fprintf fmt "<"
  | Le -> Format.fprintf fmt "<="
  | Gt -> Format.fprintf fmt ">"
  | Ge -> Format.fprintf fmt ">="
;;

let pp_rec_flag fmt = function
  | Rec -> Format.fprintf fmt "rec "
  | NonRec -> ()
;;

let rec pp fmt = function
  | EConst n when n < 0 -> Format.fprintf fmt "(%d)" n
  | EConst n -> Format.fprintf fmt "%d" n
  | EVar x -> Format.fprintf fmt "%s" x
  | EBinop (op, l, r) -> Format.fprintf fmt "(%a %a %a)" pp l pp_binop op pp r
  | EIf (c, t, e) -> Format.fprintf fmt "(if %a then %a else %a)" pp c pp t pp e
  | EFun (x, body) -> Format.fprintf fmt "(fun %s -> %a)" x pp body
  | EApp (f, arg) -> Format.fprintf fmt "(%a %a)" pp f pp arg
  | ELet (rf, x, e1, e2) ->
    Format.fprintf fmt "(let %a%s = %a in %a)" pp_rec_flag rf x pp e1 pp e2
;;

let to_string e = Format.asprintf "%a" pp e
