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
  | PTuple of pattern list
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec
  | Rec
[@@deriving show { with_path = false }]

(** Unary operators *)
type unop =
  | Neg (** '-' *)
  | Not (** 'not' **)
[@@deriving show { with_path = false }]

type typ =
  | TInt (** Integer type *)
  | TBool (** Boolean type *)
  | TUnit (** Unit type *)
  | TFun of typ * typ (** Function type: T1 -> T2 *)
  | TTuple of typ list (** Tuple type: T1 * T2 * ... * Tn *)
  | TClass of name (** Class with a name *)
[@@deriving show { with_path = false }]

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
  | Tuple of expr list (** Tuple, e.g. `(e1, e2, ..., en)` *)
  | New of name * expr list
  (** Class instantiation with constructor args, e.g. `new className args` *)
  | MethodCall of expr * name * expr list (** Method call, e.g. `obj#methodName(args)` *)
  | FieldAccess of expr * name (** Field access, e.g. `obj#fieldName` *)
[@@deriving show { with_path = false }]

and method_def =
  { method_name : name
  ; method_params : pattern list
  ; method_body : expr
  }
[@@deriving show { with_path = false }]

and field_def = name * expr [@@deriving show { with_path = false }]

and class_def =
  { class_name : name
  ; parent_class : name option
  ; self_name : name option
  ; fields : field_def list
  ; methods : method_def list
  }
[@@deriving show { with_path = false }]

type binding = rec_flag * pattern * expr [@@deriving show { with_path = false }]

type structure_item =
  | Value of binding
  | ClassDef of class_def
[@@deriving show { with_path = false }]

type program = structure_item list [@@deriving show { with_path = false }]

let rec pp_pattern fmt = function
  | PVar s -> Format.fprintf fmt "%s" s
  | PAny -> Format.fprintf fmt "_"
  | PUnit -> Format.fprintf fmt "()"
  | PTuple ps ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_pattern)
      ps
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
      | Not -> "not "
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
    let pp_param fmt p =
      match p with
      | PVar _ | PAny | PUnit -> pp_pattern fmt p
      | PTuple _ -> Format.fprintf fmt "(%a)" pp_pattern p
    in
    Format.fprintf
      fmt
      "(fun %a -> %a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_param)
      params
      pp_expr
      body
  | App (f, arg) -> Format.fprintf fmt "(%a %a)" pp_expr f pp_expr arg
  | Let (rec_flag, p, e1, e2) ->
    let rec_str = if rec_flag = Rec then " rec" else "" in
    Format.fprintf fmt "(let%s %a = %a in %a)" rec_str pp_pattern p pp_expr e1 pp_expr e2
  | Tuple es ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
      es
  | New (class_name, args) ->
    if List.length args = 0
    then Format.fprintf fmt "(new %s)" class_name
    else
      Format.fprintf
        fmt
        "(new %s %a)"
        class_name
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_expr)
        args
  | MethodCall (obj, method_name, args) ->
    Format.fprintf
      fmt
      "(%a#%s %a)"
      pp_expr
      obj
      method_name
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_expr)
      args
  | FieldAccess (obj, field_name) -> Format.fprintf fmt "(%a#%s)" pp_expr obj field_name

and pp_method_def fmt { method_name; method_params; method_body } =
  Format.fprintf
    fmt
    "method %s %a = %a"
    method_name
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_pattern)
    method_params
    pp_expr
    method_body

and pp_field_def fmt (name, init_expr) =
  Format.fprintf fmt "val %s = %a" name pp_expr init_expr

and pp_class_def fmt { class_name; parent_class; self_name; fields; methods } =
  Format.fprintf fmt "class %s = object" class_name;
  (match self_name with
   | Some self -> Format.fprintf fmt " (%s)" self
   | None -> ());
  (match parent_class with
   | Some parent -> Format.fprintf fmt "\n  inherit %s" parent
   | None -> ());
  Format.fprintf fmt "\n";
  List.iter (fun f -> Format.fprintf fmt "  %a\n" pp_field_def f) fields;
  List.iter (fun m -> Format.fprintf fmt "  %a\n" pp_method_def m) methods;
  Format.fprintf fmt "end"
;;

let pp_program_item fmt = function
  | Value (rec_flag, p, e) ->
    let rec_str = if rec_flag = Rec then " rec" else "" in
    Format.fprintf fmt "let%s %a = %a" rec_str pp_pattern p pp_expr e
  | ClassDef class_def -> pp_class_def fmt class_def
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
