open Base
open Base.Printf

type ty =
  | Tty_var of Ident.t
  | Tty_arrow of ty * ty
  | Tty_prod of ty * ty * ty list
  | Tty_constr of ty list * Ident.t

let show_tuple ?(sep = ", ") show_item (a, b, xs) =
  String.concat ~sep (List.map ~f:(fun x -> show_item x) (a :: b :: xs))
  |> sprintf "@[(%s)@]"
;;

let rec show_ty = function
  | Tty_var ident -> ident.name
  | Tty_arrow (a, b) -> sprintf "(%s -> %s)" (show_ty a) (show_ty b)
  | Tty_prod (a, b, xs) -> show_tuple show_ty (a, b, xs)
  | Tty_constr ([], ident) -> sprintf "%s" ident.name
  | Tty_constr ([ ty ], ident) -> sprintf "%s %s" (show_ty ty) ident.name
  | Tty_constr (tys, ident) ->
    sprintf "(%s) %s" ident.name (String.concat ~sep:", " (List.map ~f:show_ty tys))
;;

let pp_ty ppf ty = Format.fprintf ppf "%s" (show_ty ty)

module VarSet = struct
  include Stdlib.Set.Make (Ident)
end

module Scheme = struct
  type t = Scheme of VarSet.t * ty
end

type pattern =
  | Tpat_any
  | Tpat_const of Parsetree.constant
  | Tpat_var of Ident.t
  | Tpat_tuple of pattern * pattern * pattern list
  | Tpat_constr of string * Ident.t * pattern option

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

module TypeEnv = struct
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

  let empty =
    { env_values = Ident.Ident_map.empty
    ; env_types = Ident.Ident_map.empty
    ; env_constructors = Ident.Ident_map.empty
    }
  ;;

  let add_type td t =
    let env_types = Ident.Ident_map.add td.tty_ident td t.env_types in
    { t with env_types }
  ;;

  let add_constructor constr t =
    let env_constructors =
      Ident.Ident_map.add constr.constr_ident constr t.env_constructors
    in
    { t with env_constructors }
  ;;

  let add_value ~ident scheme t =
    let env_values = Ident.Ident_map.add ident scheme t.env_values in
    { t with env_values }
  ;;
end

type structure_item =
  | Tstr_value of value_binding
  | Tstr_type of type_declaration

type structure = (TypeEnv.t * structure_item) list
