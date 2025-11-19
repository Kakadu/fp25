[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast
open Utils

let pp_brujin (* ?(compact = true) *) =
  let rec pp fmt = function
    | EApp (EApp (EVar (Index 0), l), r) -> Format.fprintf fmt "(%a + %a)" pp l pp r
    | EApp (EApp (EVar (Index 1), l), r) -> Format.fprintf fmt "(%a - %a)" pp l pp r
    | EApp (EApp (EVar (Index 2), l), r) -> Format.fprintf fmt "(%a * %a)" pp l pp r
    | EApp (EApp (EVar (Index 3), l), r) -> Format.fprintf fmt "(%a / %a)" pp l pp r
    | EIf (pred, e1, e2) ->
      Format.fprintf fmt "if (%a) then (%a) else (%a)" pp pred pp e1 pp e2
    | EVar (Index i) -> Format.fprintf fmt "i%d" i
    | EApp (l, r) -> Format.fprintf fmt "(%a %a)" pp l pp r
    | EConst (Int i) -> Format.fprintf fmt "%d" i
    | EConst (Bool b) -> Format.fprintf fmt (if b then "true" else "false")
    | EAbs (_, t) -> Format.fprintf fmt "(λ . %a)" pp t
    | ELet (NotRecursive, Index i, e1, e2) ->
      Format.fprintf fmt "let i%d = %a in %a" i pp e1 pp e2
    | ELet (Recursive, Index i, e1, e2) ->
      Format.fprintf fmt "let rec i%d = %a in %a" i pp e1 pp e2
  in
  pp
;;

let pp ?(compact = true) =
  let open Format in
  let mangle t fmt x =
    if is_free_in x t || not compact then fprintf fmt "%s" x else fprintf fmt "_"
  in
  let rec pp fmt = function
    | EVar s -> Format.fprintf fmt "%s" s
    | EApp (l, r) -> Format.fprintf fmt "(%a %a)" pp l pp r
    | EConst (Int i) -> Format.fprintf fmt "%d" i
    | EIf (pred, e1, e2) -> Format.fprintf fmt "if %a then %a else %a" pp pred pp e1 pp e2
    | EConst (Bool b) -> Format.fprintf fmt (if b then "true" else "false")
    | ELet (NotRecursive, v, e1, e2) ->
      Format.fprintf fmt "let %s = %a in %a" v pp e1 pp e2
    | ELet (Recursive, v, e1, e2) ->
      Format.fprintf fmt "let rec %s = %a in %a" v pp e1 pp e2
    (* | Abs (x, Abs (y, Var z)) when x = z && y <> z && compact -> *)
    (*   if compact then Format.fprintf fmt "⊤" *)
    (* | Abs (x, Abs (y, Var z)) when y = z && x <> z && compact -> Format.fprintf fmt "⊥" *)
    (* | Abs (f, Abs (x, Var z)) when x = z && x <> f && compact -> Format.fprintf fmt "0" *)
    (* | Abs (f, Abs (x, App (Var g, Var z))) when x = z && x <> f && g = f && compact -> *)
    (*   Format.fprintf fmt "1" *)
    (* | Abs (f, Abs (x, App (Var g, App (Var h, Var z)))) *)
    (*   when x = z && x <> f && g = f && h = g && compact -> Format.fprintf fmt "2" *)
    (* | EBop (Plus, a, b) -> Format.fprintf fmt "%a + %a" pp a pp b *)
    (* | EBop (Minus, a, b) -> Format.fprintf fmt "%a - %a" pp a pp b *)
    (* | EBop (Asterisk, a, b) -> Format.fprintf fmt "%a * %a" pp a pp b *)
    (* | EBop (Slash, a, b) -> Format.fprintf fmt "%a / %a" pp a pp b *)
    (* | EBop (Other s, a, b) -> Format.fprintf fmt "%a %c %a" pp a s pp b *)
    | EAbs (v1, EAbs (v2, EAbs (v3, EAbs (v4, t)))) when compact ->
      Format.fprintf
        fmt
        "(λ %a %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        (mangle t)
        v4
        pp
        t
    | EAbs (v1, EAbs (v2, EAbs (v3, t))) when compact ->
      Format.fprintf
        fmt
        "(λ %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        pp
        t
    | EAbs (v1, EAbs (v2, t)) when compact ->
      Format.fprintf fmt "(λ %a %a -> %a)" (mangle t) v1 (mangle t) v2 pp t
    | EAbs (x, t) -> Format.fprintf fmt "(λ %a . %a)" (mangle t) x pp t
  in
  pp
;;

let pp_hum = pp ~compact:true
let pp = pp ~compact:false
let pp_brujin_compact = pp_brujin (* ~compact:true *)
let pp_brujin = pp_brujin (* ~compact:false *)
