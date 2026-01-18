[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type rec_flag =
  | Recursive (** for [ let rec patt = expr ] *)
  | NonRecursive (** for [ let patt = expr ] *)

val show_rec_flag : rec_flag -> string

type 'a list1 = 'a * 'a list (* non-empty list *)

type constant =
  | CUnit (** [ () ]*)
  | CInt of int (** [ 666 ] *)
  | CBool of bool (** [ true, false ] *)

val show_constant : constant -> string

type binop =
  | Add (** [ + ] *)
  | Mul (** [ * ] *)
  | Sub (** [ - ] *)
  | Div (** [ / ] *)
  | Eq (** [ == ] *)
  | Ne (** [ <> ] *)
  | Le (** [ <= ] *)
  | Ge (** [ >= ] *)
  | Lt (** [ < ] *)
  | Gt (** [ > ] *)

val show_binop : binop -> string

type pattern =
  | PAny (** [ _ ] *)
  | PConstant of constant (** [ PConstant 666 ] *)
  | PVar of string (** [ PVar "var_name" ] *)
  | PTuple of pattern * pattern * pattern list
  (** [ (patt1, patt2, [ patt3; ...; pattN ]) ] *)
  | PConstruct of string * pattern option (** [ Some 666, None, Ok (1, 2, 3) ] etc. *)

val show_pattern : pattern -> string
val pp_pattern : Format.formatter -> pattern -> unit

type expression =
  | EConstant of constant (** [ EConstant 666 ] *)
  | EVar of string (** [ EVar "var_name" ] *)
  | ETuple of expression * expression * expression list
  (** [ (expr1, expr2, [ expr3; ...; exprN ]) ] *)
  | EBinop of binop * expression * expression (** [ expr1 * expr2 ] *)
  | ELet of rec_flag * (pattern * expression) list1 * expression
  (** [ let patt1 = expr1 and patt2 = expr2 and ... and pattN = exprN in expr ] *)
  | EFun of pattern * expression (** [ fun pattern -> expression ] *)
  | EIf of expression * expression * expression (** [ if expr1 then expr2 else expr3 ] *)
  | EApp of expression * expression (** [ expr1 expr2 ] *)
  | EConstruct of string * expression option (** [ Some 666, None, Ok (1, 2, 3) ] etc. *)
  | EMatch of expression * (pattern * expression) list1
  (** [ match expession with | patt1 -> expr1 ... | pattN -> exprN ] *)

val show_expression : expression -> string
val pp_expression : Format.formatter -> expression -> unit

(** [ let pattern = expression ] *)
type value_binding = pattern * expression

type core_type =
  | Pty_var of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | Pty_arrow of core_type * core_type (** ['a -> 'b] *)
  | Pty_tuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | Pty_constr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

val show_core_type : core_type -> string
val pp_core_type : Format.formatter -> core_type -> unit

type type_kind =
  | Pty_abstract of core_type option (** [ type t ], [ type t = x ] *)
  | Pty_variants of (string * core_type option) list1
  (** [ type t = Some of int | None ] *)

type type_declaration =
  { pty_params : string list (** ['a] is param in [type 'a list = ...] *)
  ; pty_name : string (** [list] is name in [type 'a list = ...] *)
  ; pty_kind : type_kind (** abstract, adt, record etc. *)
  }

type structure_item =
  | Pstr_value of rec_flag * value_binding list1 (** [ let x = ... ] *)
  | Pstr_type of type_declaration list1 (** [ type x = ... ] *)

(** [ (stru_item1, [ stru_item2; ...; stru_itemN ]) ] *)
type structure = structure_item list1

val show_structure : structure -> string
val pp_structure : Format.formatter -> structure -> unit
