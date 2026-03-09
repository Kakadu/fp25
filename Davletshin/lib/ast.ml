[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** recursive and not recursive flag *)
type flag =
  | Rec (** Recursive *)
  | Nonrec (** Non recursive *)
[@@deriving eq, show]

(** binary operators *)
type bop =
  | Plus (** Addition [+] *)
  | Minus (** Subtraction [-] *)
  | Times (** Multiplication [*] *)
  | Divide (** Division [/] *)
  | Eq (** Equal [=] *)
  | Neq (** Not equal [<>] *)
  | Lt (** Less than [<] *)
  | Gt (** Greater than [>] *)
  | Le (** Less or equal [<=] *)
  | Ge (** Greater or equal [>=] *)
[@@deriving eq, show]

type name = string

(** The main type for our abstract syntax tree *)
type 'name t =
  | Int of int (** Integer literal [] *)
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [fun x -> t] *)
  | App of 'name t * 'name t (** Application [f g] *)
  | Binop of bop * 'name t * 'name t (** Binary operator [a op b] *)
  | Neg of 'name t (** Negative operator [-e] *)
  | If of 'name t * 'name t * 'name t (** Condition [if c then t else e] *)
  | Let of flag * 'name * 'name t * 'name t (** Let binding [let [rec] p = e1 in e2] *)
  | Fix of 'name t (** Fix-point combinator [fix (fun self -> e)]*)
[@@deriving eq, show]
