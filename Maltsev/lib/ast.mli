type num = int
type ident = string
type recflag = bool

type binop =
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Neq
  | Le
  | Bi
  | Leq
  | Beq

type arithm =
  | Const of num (* binary expr terminates in integer/some identifier *)
  | Ident of ident (* variable used in arithmetic *)
  | Binexpr of binop * arithm * arithm

type expr =
  | Const of num (* integer *)
  | Ident of ident (* some variable name to lookup *)
  | Binexpr of binop * expr * expr (* arithmetic or comparison of 2 expressions *)
  | Ite of expr * expr * expr (* condition (false is 0 otherwise true), then, else *)
  | Abs of ident * expr (* curried lambda abstraction (fun name -> expr) *)
  | App of expr * expr (* application *)
  | Let of recflag * ident * expr * expr option
(* let binding, might be recursive, might be just a function definition *)
