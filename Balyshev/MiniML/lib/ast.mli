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

type type_declaration =
  { typedef_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; typedef_name : string (** [list] is name in [type 'a list = ...]  *)
  ; typedef_kind : type_kind
  }

and type_kind =
  | KAbstract of core_type option (** [ type t ], [ type t = x ] *)
  | KVariants of (string * core_type option) list1 (** [ type t = Some of int | None ]  *)

and core_type =
  | CTVar of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | CTArrow of core_type * core_type (** ['a -> 'b] *)
  | CTTuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | CTConstr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

type structure_item =
  | SValue of value_binding (** [ let x = ... ] *)
  | SType of type_declaration list1 (** [ type x = ... ] *)

type structure = structure_item list1

val show_constant : constant -> string
val pp_constant : Format.formatter -> constant -> unit
val show_expression : expression -> string
val pp_expression : Format.formatter -> expression -> unit
val show_pattern : pattern -> string
val pp_pattern : Format.formatter -> pattern -> unit
val show_structure : structure -> string
val pp_structure : Format.formatter -> structure -> unit

type ty =
  | TUnit
  | TInt
  | TBool
  | TVar of string
  | TArrow of ty * ty
  | TProd of ty * ty * ty list

val show_ty : ty -> string
val pp_ty : Format.formatter -> ty -> unit

(* testing stuff *)
val pp_core_type : Format.formatter -> core_type -> unit
