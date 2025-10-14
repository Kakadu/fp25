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
  | If of expr * expr * expr (** Conditional expression, e.g. `if e1 then e2 else e3` *)
  | Let of name * expr * expr (** Local binding, e.g. `let x = e1 in e2` *)
  | LetRec of name * expr * expr (** Recursive binding, e.g. `let rec f = e1 in e2` *)
  | Fun of name * expr (** Abstraction (function), e.g. `fun x -> e` *)
  | App of expr * expr (** Function application, e.g. `f x` *)
[@@deriving show { with_path = false }]
