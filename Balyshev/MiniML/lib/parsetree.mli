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

type value_binding = pattern * expression

type type_declaration =
  { pty_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; pty_name : string (** [list] is name in [type 'a list = ...]  *)
  ; pty_kind : type_kind
  }

and type_kind =
  | Pty_abstract of core_type option (** [ type t ], [ type t = x ] *)
  | Pty_variants of (string * core_type option) list1
  (** [ type t = Some of int | None ]  *)

and core_type =
  | Pty_var of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | Pty_arrow of core_type * core_type (** ['a -> 'b] *)
  | Pty_tuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | Pty_constr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

type structure_item =
  | Pstr_value of rec_flag * value_binding list1 (** [ let x = ... ] *)
  | Pstr_type of type_declaration list1 (** [ type x = ... ] *)

type structure = structure_item list1

val show_tuple : ?sep:string -> ('a -> string) -> 'a * 'a * 'a list -> string
val show_rec_flag : rec_flag -> string
val show_constant : constant -> string
val pp_constant : Format.formatter -> constant -> unit
val show_binop : binop -> string
val pp_binop : Format.formatter -> binop -> unit
val show_expression : expression -> string
val pp_expression : Format.formatter -> expression -> unit
val show_pattern : pattern -> string
val pp_pattern : Format.formatter -> pattern -> unit
val show_structure : structure -> string
val pp_structure : Format.formatter -> structure -> unit

(* testing stuff *)
val pp_core_type : Format.formatter -> core_type -> unit
