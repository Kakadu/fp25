type const = IConst of int | FConst of float | BConst of bool
[@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt
  | And
  | Or
  | AddF
  | SubF
  | MulF
  | DivF
[@@deriving show]

type wordtoken = Keyword of string | Word of string [@@deriving show]
type reclabel = Recursive | Nonrecursive [@@deriving show]

type expr =
  | EConst of const
  | EVar of string
  | EBinOp of binop * expr * expr
  | ELet of reclabel * letbind * expr
  | EIf of expr * expr * expr
  | EFun of expr * expr
  | EApp of expr * expr

and letbind = Bind of expr * expr [@@deriving show]
