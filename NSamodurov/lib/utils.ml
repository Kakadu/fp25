[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast

(* TODO: use a set instead of list *)
let list_remove x ~equal = List.filter ~f:(fun a -> not (equal a x))

let free_vars ~equal =
  let rec helper acc = function
    | EVar s -> s :: acc
    | EConst _ -> []
    | ELet (_, _, e1, e2) -> helper (helper acc e1) e2
    | EAbs (s, l) -> acc @ list_remove ~equal s (helper [] l)
    | EApp (l, r) -> helper (helper acc r) l
    | EBop (_, a, b) -> helper (helper acc a) b
  in
  helper []
;;

let is_free_in x term =
  List.mem (free_vars term ~equal:String.equal) x ~equal:String.equal
;;

(* let brujin_equal x y = *)
(*   match x, y with *)
(*   | Index x, Index y -> Int.equal x y *)
(*   | _ -> false *)
(* ;; *)

(* let is_free_in_brujin x term = *)
(*   List.mem (free_vars term ~equal:brujin_equal) x ~equal:brujin_equal *)
(* ;; *)

let var x = EVar x
let abs x l = EAbs (x, l)
let app l r = EApp (l, r)
