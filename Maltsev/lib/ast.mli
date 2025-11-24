type num = int
type ident = string
type recflag = bool

type binop =
  | Plus
  | Minus
  | Mul
  | Div

type compop =
  | Eq
  | Neq
  | Le
  | Bi
  | Leq
  | Beq

type arithm =
  | Const of num (* binary expr terminates in integer/some identifier *)
  | Ident of ident
  | Binexpr of binop * arithm * arithm

type comparison = Comp of compop * arithm * arithm

type expr =
  | Var of string
  | Ite of comparison * expr * expr
  | Abs of ident * expr
  | App of expr * expr
  | Let of recflag * ident * expr * expr option
