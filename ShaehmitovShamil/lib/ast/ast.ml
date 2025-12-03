(** Name of function or variable *)
type name = string [@@deriving show { with_path = false }]

(** Binary operators *)
type binop =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
[@@deriving show { with_path = false }]

type constant =
  | CInt of int
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
type unop = Neg (** - *) [@@deriving show { with_path = false }]

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

type structure_item =
  | Value of binding
  | Expr of expr
[@@deriving show { with_path = false }]

type program = structure_item list [@@deriving show { with_path = false }]

let pretty_print_pattern = function
  | PVar s -> s
  | PAny -> "_"
  | PUnit -> "()"
;;

let rec pretty_print_expr = function
  | Const (CInt i) -> string_of_int i
  | Const CUnit -> "()"
  | Var s -> s
  | UnOp (op, e) ->
    let op_str =
      match op with
      | Neg -> "-"
    in
    "(" ^ op_str ^ pretty_print_expr e ^ ")"
  | BinOp (op, e1, e2) ->
    let op_str =
      match op with
      | Add -> " + "
      | Sub -> " - "
      | Mul -> " * "
      | Div -> " / "
    in
    "(" ^ pretty_print_expr e1 ^ op_str ^ pretty_print_expr e2 ^ ")"
  | If (cond, t, f) ->
    "(if "
    ^ pretty_print_expr cond
    ^ " then "
    ^ pretty_print_expr t
    ^ " else "
    ^ pretty_print_expr f
    ^ ")"
  | FunExpr (param, body) ->
    "(fun "
    ^ String.concat " " (List.map pretty_print_pattern param)
    ^ " -> "
    ^ pretty_print_expr body
    ^ ")"
  | App (f, arg) -> "(" ^ pretty_print_expr f ^ " " ^ pretty_print_expr arg ^ ")"
  | Let (NonRec, p, e1, e2) ->
    "(let "
    ^ pretty_print_pattern p
    ^ " = "
    ^ pretty_print_expr e1
    ^ " in "
    ^ pretty_print_expr e2
    ^ ")"
  | Let (Rec, p, e1, e2) ->
    "(let rec "
    ^ pretty_print_pattern p
    ^ " = "
    ^ pretty_print_expr e1
    ^ " in "
    ^ pretty_print_expr e2
    ^ ")"
;;

let pretty_print_program_item = function
  | Value (NonRec, p, e) -> "let " ^ pretty_print_pattern p ^ " = " ^ pretty_print_expr e
  | Value (Rec, p, e) -> "let rec " ^ pretty_print_pattern p ^ " = " ^ pretty_print_expr e
  | Expr e -> pretty_print_expr e
;;

let pretty_print_program p =
  String.concat ";;\n" (List.map pretty_print_program_item p) ^ ";;\n"
;;
