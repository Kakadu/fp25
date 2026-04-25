type const =
  | IConst of int
  | FConst of float
  | BConst of bool

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

type reclabel =
  | Recursive
  | Nonrecursive

type expr =
  | EConst of const
  | EVar of string
  | EBinOp of binop * expr * expr
  | ELet of reclabel * letbind * expr
  | EIf of expr * expr * expr
  | EFun of expr * expr
  | EApp of expr * expr

and letbind = Bind of expr * expr

type toplevel =
  | TopLet of reclabel * letbind
  | TopExpr of expr

val pp_const : Format.formatter -> const -> unit
val pp_binop : Format.formatter -> binop -> unit
val pp_reclabel : Format.formatter -> reclabel -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_letbind : Format.formatter -> letbind -> unit
val pp_toplevel : Format.formatter -> toplevel -> unit
