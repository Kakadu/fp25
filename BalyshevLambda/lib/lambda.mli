[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(*
   val var : 'a -> 'a Ast.t
   val abs : 'a -> 'a Ast.t -> 'a Ast.t
   val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t *)

val subst : string -> by:string Ast.t -> string Ast.t -> string Ast.t

type strat =
  { on_var : strat -> Ast.name -> string Ast.t
  ; on_abs : strat -> Ast.name -> string Ast.t -> string Ast.t
  ; on_app : strat -> string Ast.t -> string Ast.t -> string Ast.t
  }

val apply_strat : strat -> string Ast.t -> string Ast.t
val without_strat : strat

(** Predefined strategies *)

val cbn_strat : strat
val nor_strat : strat
val cbv_strat : strat
val ao_strat : strat

(** Example lambda expressions *)

val a : string Ast.t
val x : string Ast.t
val y : string Ast.t
val z : string Ast.t
val f : string Ast.t
val g : string Ast.t
val h : string Ast.t
val m : string Ast.t
val n : string Ast.t
val p : string Ast.t
val zero : string Ast.t
val one : string Ast.t
val two : string Ast.t
val three : string Ast.t

(* my garbage *)

val ao_small_step_strat : strat

(*
   type 'a limited_t =
  | Over of 'a
  | NotOver of 'a * int

type limited_strat =
  { on_var : limited_strat -> Ast.name limited_t -> string Ast.t limited_t
  ; on_abs :
      limited_strat -> (Ast.name * string Ast.t) limited_t -> string Ast.t limited_t
  ; on_app :
      limited_strat -> (string Ast.t * string Ast.t) limited_t -> string Ast.t limited_t
  }

val apply_limited_strat
  :  limited_strat
  -> Ast.name Ast.t limited_t
  -> Ast.name Ast.t limited_t *)

type 'a limited_t =
  | Over of 'a
  | NotOver of 'a * int

type limited_expr = Ast.name Ast.t limited_t

type limited_strat =
  { on_var : limited_strat -> Ast.name limited_t -> limited_expr
  ; on_abs : limited_strat -> (Ast.name * string Ast.t) limited_t -> limited_expr
  ; on_app : limited_strat -> (string Ast.t * string Ast.t) limited_t -> limited_expr
  }

val ao_limited : limited_strat
val cbn_limited : limited_strat
val apply_limited_strat : limited_strat -> limited_expr -> limited_expr
val set_lim : 'a -> int -> 'a limited_t
