[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

let list_remove x = List.filter ~f:(fun a -> not (String.equal a x))

let free_vars =
  let rec helper acc = function
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
    | Const _ -> acc
    | If (c, t, e) -> helper (helper (helper acc c) t) e
    | Let (x, e1, e2) -> helper (acc @ list_remove x (helper [] e2)) e1
    | LetRec (f, x, body, e) ->
      acc @ list_remove f (list_remove x (helper (helper [] body) e))
    | Fix t -> helper acc t
    | BinOp (_, l, r) -> helper (helper acc l) r
    | Prim (_, args) -> List.fold_left ~f:helper ~init:acc args
  in
  helper []
;;

let is_free_in x term = List.mem (free_vars term) x ~equal:String.equal

let pp ?(compact = false) =
  let open Stdlib.Format in
  let mangle t fmt x =
    if is_free_in x t || not compact then fprintf fmt "%s" x else fprintf fmt "_"
  in
  let rec pp_impl fmt = function
    | Var s -> fprintf fmt "%s" s
    | App (l, r) -> fprintf fmt "(%a %a)" pp_impl l pp_impl r
    (* True: λx.λy.x *)
    | Abs (x, Abs (y, Var z)) when String.equal x z && (not (String.equal y z)) && compact
      -> fprintf fmt "⊤"
    (* False: λx.λy.y *)
    | Abs (x, Abs (y, Var z)) when String.equal y z && (not (String.equal x z)) && compact
      -> fprintf fmt "⊥"
    (* Zero: λf.λx.x *)
    | Abs (f, Abs (x, Var z)) when String.equal x z && (not (String.equal x f)) && compact
      -> fprintf fmt "0"
    (* One: λf.λx.f x *)
    | Abs (f, Abs (x, App (Var g, Var z)))
      when String.equal x z && (not (String.equal x f)) && String.equal g f && compact ->
      fprintf fmt "1"
    (* Two: λf.λx.f (f x) *)
    | Abs (f, Abs (x, App (Var g, App (Var h, Var z))))
      when String.equal x z
           && (not (String.equal x f))
           && String.equal g f
           && String.equal h g
           && compact -> fprintf fmt "2"
    (* Лямбда с 4 параметрами в компактном режиме *)
    | Abs (v1, Abs (v2, Abs (v3, Abs (v4, t)))) when compact ->
      fprintf
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
        pp_impl
        t
    (* Лямбда с 3 параметрами в компактном режиме *)
    | Abs (v1, Abs (v2, Abs (v3, t))) when compact ->
      fprintf fmt "(λ %a %a %a -> %a)" (mangle t) v1 (mangle t) v2 (mangle t) v3 pp_impl t
    (* Лямбда с 2 параметрами в компактном режиме *)
    | Abs (v1, Abs (v2, t)) when compact ->
      fprintf fmt "(λ %a %a -> %a)" (mangle t) v1 (mangle t) v2 pp_impl t
    | Abs (x, t) when compact -> fprintf fmt "(λ %a . %a)" (mangle t) x pp_impl t
    (* Обычная лямбда (не компактный режим) *)
    | Abs (x, body) -> fprintf fmt "(fun %s -> %a)" x pp_impl body
    | Const n -> fprintf fmt "%d" n
    | If (c, t1, t2) ->
      fprintf fmt "(if %a then %a else %a)" pp_impl c pp_impl t1 pp_impl t2
    | Let (n, e1, e2) -> fprintf fmt "(let %s = %a in %a)" n pp_impl e1 pp_impl e2
    | LetRec (f, p, b, inb) ->
      fprintf fmt "(let rec %s %s = %a in %a)" f p pp_impl b pp_impl inb
    | Fix t -> fprintf fmt "(fix %a)" pp_impl t
    | BinOp (op, l, r) ->
      let s =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "="
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Ge -> ">="
      in
      fprintf fmt "(%a %s %a)" pp_impl l s pp_impl r
    | Prim (name, args) ->
      fprintf fmt "%s(%a)" name (pp_print_list ~pp_sep:pp_print_space pp_impl) args
  in
  pp_impl
;;

let pp_hum = pp ~compact:true
let pp_verbose = pp ~compact:false

let show ?(compact = false) term =
  let buf = Buffer.create 256 in
  let fmt = Stdlib.Format.formatter_of_buffer buf in
  pp ~compact fmt term;
  Stdlib.Format.pp_print_flush fmt ();
  Buffer.contents buf
;;
