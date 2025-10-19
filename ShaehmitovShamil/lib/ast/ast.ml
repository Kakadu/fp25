(** Name of function or variable *)
type name = string [@@deriving show { with_path = false }]

(** Binary operators *)
type binop =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | Neq (** <> *)
  | Lt (** < *)
  | Gt (** > *)
  | Le (** <= *)
  | Ge (** >= *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

(** Unary operators *)
type unop =
  | Neg (** - *)
  | Not (** not *)
[@@deriving show { with_path = false }]

(** Expressions in the AST *)
type expr =
  | Int of int (** Integer literal, e.g. `123` *)
  | Bool of bool (** Boolean literal, e.g. `true` *)
  | Var of name (** Variable, e.g. `x` *)
  | BinOp of binop * expr * expr (** Binary operation, e.g. `e1 + e2` *)
  | UnOp of unop * expr (** Unary operation, e.g. `-e` or `not e` *)
  | If of expr * expr * expr (** Conditional expression, e.g. `if e1 then e2 else e3` *)
  | Let of name * expr * expr (** Local binding, e.g. `let x = e1 in e2` *)
  | LetRec of name * expr * expr (** Recursive binding, e.g. `let rec f = e1 in e2` *)
  | Fun of name * expr (** Abstraction (function), e.g. `fun x -> e` *)
  | App of expr * expr (** Function application, e.g. `f x` *)
[@@deriving show { with_path = false }]

let rec pretty_print_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var s -> s
  | UnOp (op, e) ->
    let op_str =
      match op with
      | Neg -> "-"
      | Not -> "not "
    in
    "(" ^ op_str ^ pretty_print_expr e ^ ")"
  | BinOp (op, e1, e2) ->
    let op_str =
      match op with
      | Add -> " + "
      | Sub -> " - "
      | Mul -> " * "
      | Div -> " / "
      | Eq -> " = "
      | Lt -> " < "
      | Gt -> " > "
      | Le -> " <= "
      | Ge -> " >= "
      | Neq -> " <> "
      | And -> " && "
      | Or -> " || "
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
  | Fun (param, body) -> "(fun " ^ param ^ " -> " ^ pretty_print_expr body ^ ")"
  | App (f, arg) -> "(" ^ pretty_print_expr f ^ " " ^ pretty_print_expr arg ^ ")"
  | Let (v, e1, e2) ->
    "(let " ^ v ^ " = " ^ pretty_print_expr e1 ^ " in " ^ pretty_print_expr e2 ^ ")"
  | LetRec (v, e1, e2) ->
    "(let rec " ^ v ^ " = " ^ pretty_print_expr e1 ^ " in " ^ pretty_print_expr e2 ^ ")"
;;
