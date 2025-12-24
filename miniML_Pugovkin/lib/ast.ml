type ident = string
[@@deriving show { with_path = false }, eq]

(** Рекурсивность связывания в let *)
type rec_flag = 
  | Nonrec (** [let x = e1 in e2] *)
  | Rec (** [let rec f = e1 in e2] *)
[@@deriving show { with_path = false }, eq]

(** Унарные операции *)
type unop = 
  | UMinus (** [-(e)] *)
  | UPlus (** [+(e)] *)
  | Not (** [not e] *)
[@@deriving show { with_path = false }, eq]

(** Бинарные арифметические операции *)
type arithmetic_binops =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Div (** [/] *)
  | Mul (** [*] *)
[@@deriving show { with_path = false }, eq]

(** Бинарные сравнения *)
type cmp_binops = 
  | Eq (** [=] *)
  | Neq (** [<>] *)
  | Lt (** [<] *)
  | Le (** [<=] *)
  | Gt (** [>] *)
  | Ge (** [>=] *)
[@@deriving show { with_path = false }, eq]

(** Бинарные логические операции *)
type bool_binops =
  | And (** [&&] *)
  | Or (** [||] *)
[@@deriving show { with_path = false }, eq]

(** Константы *)
type const =
  | Int of int (** Целое число *)
  | Bool of bool (** Логическая константа *)
  | Unit of unit (** [()] *)
[@@deriving show { with_path = false }, eq]

(** Узлы выражений *)
type expr =
  | Const of const (** Литерал *)
  | Var of ident (** Идентификатор *)
  | Lam of ident * expr (** Лямбда-абстракция: [fun x -> e] *)
  | App of expr * expr (** Аппликация: [e1 e2] *)
  | Let of rec_flag * ident * expr * expr (** Связывание: [let x = e1 in e2] *)
  | If of expr * expr * expr option (** Условие: [if c then t else e] *)
  | Unop of unop * expr (** Унарная операция *)
  | BinopArithmetic of arithmetic_binops * expr * expr (** Арифметика *)
  | BinopComp of cmp_binops * expr * expr (** Сравнение *)
  | BinopBool of bool_binops * expr * expr (** Логические операции *)
  | Fix of expr (** Фиксация: [fix e] *)
[@@deriving show { with_path = false }, eq]

(** Конструкции программы *)
type toplevel =
  | TLet of rec_flag * ident * expr (** Вершинное связывание *)
  | TExpr of expr (** Вершинное выражение *)
[@@deriving show { with_path = false }, eq]

(** Сама программа *)
type program = toplevel list
[@@deriving show { with_path = false }, eq]
