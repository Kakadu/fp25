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
    | Var s -> s :: acc
    | Int _ -> acc
    | BinOp (_, l, r) -> helper (helper acc r) l
    | If (c, t, e) ->
      let acc = helper acc c in
      let acc = helper acc t in
      (match e with
       | None -> acc
       | Some e -> helper acc e)
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
  in
  helper []
;;

let is_free_in x term = List.mem (free_vars term) x ~equal:String.equal
let var x = Var x
let abs x l = Abs (x, l)
let app l r = App (l, r)

(* TODO: rework this *)
module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
