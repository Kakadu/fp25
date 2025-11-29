type identificator = string [@@deriving show { with_path = false }]

type constant =
  | Const_int of int
  | Const_unit
[@@deriving show { with_path = false }]

type binary_op =
  | Plus
  | Sub
  | Mul
  | Div
[@@deriving show { with_path = false }]

type expression =
  | Expr_var of identificator
  | Expr_const of constant
  | Expr_binary_op of binary_op * expression * expression
  | Expr_conditional of expression * expression * expression
  | Expr_let_in of identificator * expression * expression
  | Expr_let_rec_in of identificator * expression * expression
  | Expr_fun of identificator * expression
  | Expr_ap of expression * expression list
  | Expr_fix of expression
[@@deriving show { with_path = false }]

type toplevel =
  | Top_let of identificator * expression
  | Top_let_rec of identificator * expression
[@@deriving show { with_path = false }]

type structure = toplevel list [@@deriving show { with_path = false }]
