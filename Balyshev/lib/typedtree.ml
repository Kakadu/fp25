open Base
open Base.Format

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
  (* TODO?? : add flag to specify what to print *)
  | Tty_var ident -> sprintf "'ty%d" ident.id
  | Tty_arrow (a, b) -> sprintf "(%s -> %s)" (show_ty a) (show_ty b)
  | Tty_prod (a, b, xs) -> show_tuple ~sep:" * " show_ty (a, b, xs)
  | Tty_constr ([], ident) -> sprintf "%s" ident.name
  | Tty_constr ([ ty ], ident) -> sprintf "%s %s" (show_ty ty) ident.name
  | Tty_constr (tys, ident) ->
    sprintf "(%s) %s" (String.concat ~sep:", " (List.map ~f:show_ty tys)) ident.name
;;

let pp_ty ppf ty = Format.fprintf ppf "%s" (show_ty ty)

module VarSet = struct
  include Stdlib.Set.Make (Ident)
end

let show_var_set_ids (var_set : VarSet.t) =
  match VarSet.elements var_set with
  | [] -> ""
  | [ x ] -> sprintf "'ty%d" x.id
  | x1 :: x2 :: xs ->
    Parsetree.show_tuple (fun (ident : Ident.t) -> sprintf "'ty%d" ident.id) (x1, x2, xs)
;;

let show_var_set_names (var_set : VarSet.t) =
  match VarSet.elements var_set with
  | [] -> ""
  | [ x ] -> x.name
  | x1 :: x2 :: xs ->
    Parsetree.show_tuple (fun (ident : Ident.t) -> ident.name) (x1, x2, xs)
;;

module Scheme = struct
  type t = Scheme of VarSet.t * ty
end

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : Parsetree.pattern
  ; tvb_body : Parsetree.expression
  ; tvb_scheme : Scheme.t
  }

type type_kind =
  | Tty_abstract of ty option
  | Tty_variants of (Ident.t * ty option) list

type type_declaration =
  { tty_ident : Ident.t
  ; tty_params : Ident.t list
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
  | Tstr_value of value_binding Parsetree.list1
  | Tstr_type of type_declaration

type structure = (TypeEnv.t * structure_item) list

open Format

let show_structure_item = function
  | Tstr_value (vb, vbs) ->
    let helper ~keyword { tvb_flag; tvb_pat; tvb_body; tvb_scheme } =
      let (Scheme.Scheme (_vs, ty)) = tvb_scheme in
      sprintf
        "%s%s%s: %s = %s@." (* TODO : replace @. with ??? *)
        keyword
        (Parsetree.show_rec_flag tvb_flag)
        (Parsetree.show_pattern tvb_pat)
        (show_ty ty)
        (Parsetree.show_expression tvb_body)
    in
    String.concat (helper ~keyword:"let" vb :: List.map ~f:(helper ~keyword:"and") vbs)
  | Tstr_type { tty_ident; tty_kind; tty_params } ->
    let kind =
      match tty_kind with
      | Tty_abstract None -> "@ "
      | Tty_abstract (Some ty) -> sprintf " = %s@ " (show_ty ty)
      | Tty_variants variants ->
        let show_variant ((ident : Ident.t), ty_opt) =
          match ty_opt with
          | None -> sprintf "| %s@ " ident.name
          | Some ty -> sprintf "| %s of %s@ " ident.name (show_ty ty)
        in
        let s = Base.String.concat (Base.List.map ~f:show_variant variants) in
        sprintf " =@ %s" s
    in
    let params =
      match tty_params with
      | [] -> " "
      | [ x ] -> sprintf " 'ty%d " x.id
      | x1 :: x2 :: xs ->
        sprintf
          " %s "
          (show_tuple (fun (ident : Ident.t) -> sprintf "'ty%d" ident.id) (x1, x2, xs))
    in
    sprintf "type%s%s %s" params tty_ident.name kind
;;

let pp_structure_item ppf stru_item = fprintf ppf "%s" (show_structure_item stru_item)

let pp_structure ppf (item1, items) =
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@.")
    pp_structure_item
    ppf
    (item1 :: items)
;;

(* debugging stuff *)

let show_scheme (Scheme.Scheme (varset, ty)) =
  sprintf
    "∀ %s . %s"
    (Base.String.concat
       ~sep:" "
       (Base.List.map ~f:Ident.show_ident (VarSet.elements varset)))
    (show_ty ty)
;;

let pp_scheme ppf scheme = fprintf ppf "%s" (show_scheme scheme)
