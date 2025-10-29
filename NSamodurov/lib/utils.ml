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
    | Var s -> s :: acc
    | Integer _ -> []
    | Abs (s, l) -> acc @ list_remove ~equal s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
  in
  helper []
;;

let is_free_in x term =
  List.mem (free_vars term ~equal:String.equal) x ~equal:String.equal
;;

let brujin_equal x y =
  match x, y with
  | Index x, Index y -> Int.equal x y
  | _ -> false
;;

let is_free_in_brujin x term =
  List.mem (free_vars term ~equal:brujin_equal) x ~equal:brujin_equal
;;

let var x = Var x
let abs x l = Abs (x, l)
let app l r = App (l, r)

(* TODO: rework this *)
module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
