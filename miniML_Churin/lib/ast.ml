[@@@warning "-A"]

type var = string [@@deriving show]

(* Реккурсивность связывания *)

type rec_flag =
  | Plain
  | Recursive
[@@deriving show]

(* Унарный минус *)

type unop = Negate [@@deriving show]

(* Бинарные операции *)

type binop =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
[@@deriving show]

(* Операции сравнения *)

type cmpop =
  | Eq (* == *)
  | Neq (* != *)
  | Lt (* < *)
  | Le (* <= *)
  | Gt (* > *)
  | Ge (* >= *)
[@@deriving show]

type literal =
  | Integer of int (* целое число *)
  | UnitVal (* () *)
[@@deriving show]

(* Выражения *)
type expr =
  | Lit of literal (* литерал *)
  | Var of var (* переменная *)
  | Lam of var * expr (* лямбда-абстракция *)
  | App of expr * expr (* применение функции *)
  | Let of rec_flag * var * expr * expr (* let-связывание *)
  | Fix of expr (* fix-комбинатор *)
  | UnOp of unop * expr (* унарная операция *)
  | BinOp of binop * expr * expr (* бинарная операция *)
  | CmpOp of cmpop * expr * expr (* операция сравнения *)
  | If of expr * expr * expr (* условное выражение *)
[@@deriving show { with_path = false }]
