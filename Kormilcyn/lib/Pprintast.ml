[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Mul -> Format.fprintf fmt "*"
  | Sub -> Format.fprintf fmt "-"
  | Div -> Format.fprintf fmt "/"
  | Leq -> Format.fprintf fmt "<="
  | Eq -> Format.fprintf fmt "="
  | Geq -> Format.fprintf fmt ">="
;;

let rec collect_fun_args acc = function
  | Fun (x, body) -> collect_fun_args (x :: acc) body
  | body -> List.rev acc, body
;;

let pp_fun_args fmt = function
  | [] -> ()
  | x :: xs ->
    Format.pp_print_string fmt x;
    List.iter (fun arg -> Format.fprintf fmt " %s" arg) xs
;;

let rec pp fmt = function
  | Var v -> Format.pp_print_string fmt v
  | Int i -> Format.pp_print_int fmt i
  | Fix -> Format.pp_print_string fmt "fix"
  | Neg e -> Format.fprintf fmt "(-%a)" pp e
  | Bin (op, l, r) -> Format.fprintf fmt "(%a %a %a)" pp l pp_binop op pp r
  | App (f, a) -> Format.fprintf fmt "(%a %a)" pp f pp a
  | Fun (x, body) ->
    let args, body = collect_fun_args [ x ] body in
    Format.fprintf fmt "(fun %a -> %a)" pp_fun_args args pp body
  | Let (x, e1, e2) -> Format.fprintf fmt "(let %s = %a in %a)" x pp e1 pp e2
  | If (c, t, e) -> Format.fprintf fmt "(if %a then %a else %a)" pp c pp t pp e
  | LetRec (f, e1, e2) -> Format.fprintf fmt "(let rec %s = %a in %a)" f pp e1 pp e2
;;

let pp_hum = pp
