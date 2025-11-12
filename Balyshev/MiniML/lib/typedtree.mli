type ty =
  | Tty_var of Ident.t
  | Tty_arrow of ty * ty
  | Tty_prod of ty * ty * ty list
  | Tty_constr of ty list * Ident.t

type pattern =
  | Tpat_any
  | Tpat_const of Parsetree.constant
  | Tpat_var of Ident.t
  | Tpat_tuple of pattern * pattern * pattern list
  | Tpat_constr of string * Ident.t * pattern option

module VarSet : sig
  include module type of Set.Make (Ident)
end

module Scheme : sig
  type t = Scheme of VarSet.t * ty
end

type expr =
  | TConst of Parsetree.constant
  | TVar of Ident.t * ty
  | TIf of expr * expr * expr * ty
  | TFun of pattern * expr * ty
  | TApp of expr * expr * ty
  | TTuple of expr * expr * expr list * ty
  | TLet of Parsetree.rec_flag * pattern * Scheme.t * expr * expr
  | TMatch of expr * (pattern * expr) Parsetree.list1 * ty
  | TConstruct of Ident.t * expr option * ty

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : pattern
  ; tvb_body : expr
  ; tvb_typ : Scheme.t
  }

type type_kind =
  | Tty_abstract of ty option
  | Tty_variants of (string * ty option) list

type type_declaration =
  { tty_ident : Ident.t
  ; tty_params : VarSet.t
  ; tty_kind : type_kind
  }

module TypeEnv : sig
  type constructor_entry =
    { constr_ident : Ident.t
    ; constr_type_ident : Ident.t
    ; constr_arg_ty : ty option
    ; constr_arity : int
    }

  type t =
    { env_constructors : constructor_entry Ident.Ident_map.t
    ; env_types : type_declaration Ident.Ident_map.t
    ; env_values : Scheme.t Ident.Ident_map.t
    }

  val empty : t
  val add_type : type_declaration -> t -> t
  val add_constructor : constructor_entry -> t -> t
  val add_value : ident:Ident.t -> Scheme.t -> t -> t
end

type structure_item =
  | Tstr_value of value_binding
  | Tstr_type of type_declaration

type structure = (TypeEnv.t * structure_item) list

val show_ty : ty -> string
val pp_ty : Format.formatter -> ty -> unit
