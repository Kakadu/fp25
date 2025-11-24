type ident = string
[@@deriving show { with_path = false }]

(** Рекурсивность связывания в let *)
type rec_flag = 
| Nonrec    (** [let x = e1 in e2] *)
| Rec       (** [let rec f = e1 in e2] *)
[@@deriving show { with_path = false }]

(** Унарные операции *)
type unop = 
| UMinus  
| UPlus
| Not
[@@deriving show { with_path = false }]

(** Бинарные арифметические операции *)
type arithmetic_binops =
| Add
| Sub
| Div
| Mul
[@@deriving show { with_path = false }]

(** Бинарные сравнения *)
type cmp_binops = 
| Eq
| Neq
| Lt
| Le
| Gt
| Ge
[@@deriving show { with_path = false }]

(** Константы *)
type const =
| Int of int
| Bool of bool
| Unit of unit
[@@deriving show { with_path = false }]

(** Узлы выражений *)
type expr =
| Const of const
| Var of ident
| Lam of ident * expr
| App of expr * expr
| Let of rec_flag * ident * expr * expr
| If of expr * expr * expr option
| Unop of unop * expr
| Binop of cmp_binops * expr * expr
| BinopArithmetic of arithmetic_binops * expr * expr
| BinopComp of cmp_binops * expr * expr
[@@deriving show { with_path = false }]

(** Конструкции программы *)
type toplevel =
| TLet of rec_flag * ident * expr
| TExpr of expr
[@@deriving show { with_path = false }]

(** Сама программа *)
type program = toplevel list
[@@deriving show { with_path = false }]