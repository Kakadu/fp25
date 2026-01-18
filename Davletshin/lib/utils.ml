[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast

(* TODO: use a set instead of list *)
let list_remove x = List.filter ~f:(fun a -> not (String.equal a x))

let free_vars =
  let rec helper acc = function
    | Int _ -> acc
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
    | Binop (_, l, r) -> helper (helper acc l) r
    | Unop (_, e) -> helper acc e
  in
  helper []
;;

let is_free_in x term = List.mem (free_vars term) x ~equal:String.equal
let int_cons n = Int n
let var x = Var x
let abs x l = Abs (x, l)
let app l r = App (l, r)

(* TODO: rework this *)
module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
