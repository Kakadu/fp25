[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Name of function or variable *)
type name = string [@@deriving show { with_path = false }]

(** Binary operators *)
type binop =
  | Add (** '+' *)
  | Sub (** '-' *)
  | Mul (** '*' *)
  | Div (** '/' *)
  | Eq (** '=' *)
  | Neq (** '<>' *)
  | Lt (** '<' *)
  | Le (** '<=' *)
  | Gt (** '>' *)
  | Ge (** '>=' *)
  | And (** '&&' *)
  | Or (** '||' *)
[@@deriving show { with_path = false }]

type constant =
  | CInt of int
  | CBool of bool
  | CUnit
[@@deriving show { with_path = false }]

type pattern =
  | PVar of name
  | PAny
  | PUnit
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec
  | Rec
[@@deriving show { with_path = false }]

(** Unary operators *)
type unop = Neg (** '-' *) [@@deriving show { with_path = false }]

(** Expressions in the AST *)
type expr =
  | Const of constant (** Constant, e.g. integer or boolean *)
  | Var of name (** Variable, e.g. `x` *)
  | BinOp of binop * expr * expr (** Binary operation, e.g. `e1 + e2` *)
  | UnOp of unop * expr (** Unary operation, e.g. `-e` or `not e` *)
  | If of expr * expr * expr (** Conditional expression, e.g. `if e1 then e2 else e3` *)
  | Let of rec_flag * pattern * expr * expr (** Local binding, e.g. `let x = e1 in e2` *)
  | FunExpr of pattern list * expr (** Abstraction (function), e.g. `fun x -> e` *)
  | App of expr * expr (** Function application, e.g. `f x` *)
[@@deriving show { with_path = false }]

type binding = rec_flag * pattern * expr [@@deriving show { with_path = false }]
type structure_item = Value of binding [@@deriving show { with_path = false }]
type program = structure_item list [@@deriving show { with_path = false }]

let pp_pattern fmt = function
  | PVar s -> Format.fprintf fmt "%s" s
  | PAny -> Format.fprintf fmt "_"
  | PUnit -> Format.fprintf fmt "()"
;;

let rec pp_expr fmt = function
  | Const (CInt i) -> Format.fprintf fmt "%d" i
  | Const (CBool b) -> Format.fprintf fmt "%b" b
  | Const CUnit -> Format.fprintf fmt "()"
  | Var s -> Format.fprintf fmt "%s" s
  | UnOp (op, e) ->
    let op_str =
      match op with
      | Neg -> "-"
    in
    Format.fprintf fmt "(%s%a)" op_str pp_expr e
  | BinOp (op, e1, e2) ->
    let op_str =
      match op with
      | Add -> " + "
      | Sub -> " - "
      | Mul -> " * "
      | Div -> " / "
      | Eq -> " = "
      | Neq -> " <> "
      | Lt -> " < "
      | Le -> " <= "
      | Gt -> " > "
      | Ge -> " >= "
      | And -> " && "
      | Or -> " || "
    in
    Format.fprintf fmt "(%a%s%a)" pp_expr e1 op_str pp_expr e2
  | If (cond, t, f) ->
    Format.fprintf fmt "(if %a then %a else %a)" pp_expr cond pp_expr t pp_expr f
  | FunExpr (params, body) ->
    Format.fprintf
      fmt
      "(fun %a -> %a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_pattern)
      params
      pp_expr
      body
  | App (f, arg) -> Format.fprintf fmt "(%a %a)" pp_expr f pp_expr arg
  | Let (rec_flag, p, e1, e2) ->
    let rec_str = if rec_flag = Rec then " rec" else "" in
    Format.fprintf fmt "(let%s %a = %a in %a)" rec_str pp_pattern p pp_expr e1 pp_expr e2
;;

let pp_program_item fmt = function
  | Value (rec_flag, p, e) ->
    let rec_str = if rec_flag = Rec then " rec" else "" in
    Format.fprintf fmt "let%s %a = %a" rec_str pp_pattern p pp_expr e
;;

let pp_program fmt p =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";;\n")
    (fun fmt item -> Format.fprintf fmt "%a" pp_program_item item)
    fmt
    p;
  if p <> [] then Format.fprintf fmt ";;\n"
;;

let pretty_print_pattern p = Format.asprintf "%a" pp_pattern p
let pretty_print_expr e = Format.asprintf "%a" pp_expr e
let pretty_print_program_item item = Format.asprintf "%a" pp_program_item item
let pretty_print_program p = Format.asprintf "%a" pp_program p
