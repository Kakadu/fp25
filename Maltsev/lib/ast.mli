type num = int (* integer alias *)
type ident = string (* string alias *)
type recflag = Recflag of bool (* flag for rec *)

type binop =
  | Plus (* + *)
  | Minus (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Eq (* = *)
  | Neq (* != *)
  | Le (* < *)
  | Bi (* > *)
  | Leq (* <= *)
  | Beq (* >= *)

type expr =
  | Const of num (* integer *)
  | Ident of ident (* some variable name to lookup *)
  | Binexpr of binop * expr * expr (* arithmetic or comparison of 2 expressions *)
  | Ite of expr * expr * expr (* condition (false is 0 otherwise true), then, else *)
  | Abs of expr * expr (* curried lambda abstraction (name and expr) *)
  | App of expr * expr (* application, fun and arg *)
  | Let of recflag * expr * expr * expr
(* let binding, might be recursive, might be just a function definition, let "rec" "name" =
   "expression" in "expression" *)
