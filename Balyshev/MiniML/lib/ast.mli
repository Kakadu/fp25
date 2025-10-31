type rec_flag =
  | Recursive
  | NonRecursive

type 'a list1 = 'a * 'a list

type constant =
  | CUnit
  | CInt of int
  | CBool of bool

and pattern =
  | PAny
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern option

type expression =
  | EConstant of constant
  | EVar of string
  | ETuple of expression * expression * expression list
  | EBinop of binop * expression * expression
  | ELet of rec_flag * (pattern * expression) list1 * expression
  | EFun of pattern * expression
  | EIf of expression * expression * expression
  | EApp of expression * expression
  | EConstruct of string * expression option
  | EMatch of expression * (pattern * expression) list1

and binop =
  | Add
  | Mul
  | Sub
  | Div
  | Cons
  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt

type value_binding = rec_flag * (pattern * expression) list1
type structure_item = SValue of value_binding
type structure = structure_item list1

val show_constant : constant -> string
val pp_constant : Format.formatter -> constant -> unit
val show_expression : expression -> string
val pp_expression : Format.formatter -> expression -> unit
val show_pattern : pattern -> string
val pp_pattern : Format.formatter -> pattern -> unit

type ty =
  | TUnit
  | TInt
  | TBool
  | TVar of string
  | TArrow of ty * ty
  | TProd of ty * ty * ty list

val show_ty : ty -> string
val pp_ty : Format.formatter -> ty -> unit
