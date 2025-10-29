type ident = string

(** Рекурсивность связывания в let *)
type rec_flag = 
| Nonrec    (** [let x = e1 in e2] *)
| Rec       (** [let rec f = e1 in e2] *)

(** Унарные операции *)
type unop = 
| UMinus  
| UPlus
| Not

(** Бинарные арифметические операции *)
type arithmetic_binops =
| Add
| Sub
| Div
| Mul

(** Бинарные сравнения *)
type cmp_binops = 
| Eq
| Neq
| Lt
| Le
| Gt
| Ge

(** Константы *)
type const =
| Int of int
| Bool of bool
| Unit of unit

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

(** Конструкции программы *)
type toplevel =
| TLet of rec_flag * ident * expr
| TExpr of expr

(** Сама программа *)
type program = toplevel list