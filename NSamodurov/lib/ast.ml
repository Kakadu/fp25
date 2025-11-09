[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Type used by AST for binary operations *)
type op =
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Other of char
[@@deriving show { with_path = false }]

(** Type used for de Brujin nameless notation *)
type brujin =
  | Index of int
  | Blank
[@@deriving show { with_path = false }]

(** Type used for let expression *)
type let_flag =
  | Recursive
  | NotRecursive
[@@deriving show { with_path = false }]

(** Type used for let expression *)
type const = Int of int [@@deriving show { with_path = false }]

(** Type used for standard notation *)
type name = string [@@deriving show { with_path = false }]

(** AST type *)
type 'a t =
  | EConst of const (** Integer [a]*)
  | EVar of 'a (** Variable: string, De brujin index [x] *)
  | ELet of let_flag * 'a * 'a t * 'a t (** Let expression [let x = e1 in e2]*)
  | EAbs of 'a * 'a t (** Abstraction [λx.e] *)
  | EApp of 'a t * 'a t (** Abstraction [N M] *)

let eint i = EConst (Int i)
let evar v = EVar v
let elet flag v e1 e2 = ELet (flag, v, e1, e2)
let eabs v t = EAbs (v, t)
let eapp e1 e2 = EApp (e1, e2)
let plus = EVar (Index 0)
let minus = EVar (Index 1)
let asterisk = EVar (Index 2)
let slash = EVar (Index 3)
let reserved = 20
let add e1 e2 = eapp (eapp (evar "+") e1) e2
let sub e1 e2 = eapp (eapp (evar "-") e1) e2
let mul e1 e2 = eapp (eapp (evar "*") e1) e2
let div e1 e2 = eapp (eapp (evar "/") e1) e2
