type rec_flag =
  | Recursive
  | NonRecursive

type 'a list1 = 'a * 'a list

type const =
  | CUnit
  | CInt of int
  | CBool of bool

and pattern =
  | PAny
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern option

type expression =
  | EConst of const
  | EVar of string
  | ETuple of expression * expression * expression list
  | EBinop of binop * expression * expression
  | ELet of rec_flag * pattern * expression * expression
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

type value_binding = rec_flag * pattern * expression
type structure_item = SValue of value_binding
type structure = structure_item list1

val show_expression : expression -> string
val pp_expression : Format.formatter -> expression -> unit
