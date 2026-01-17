open Base

type ty =
  | Tty_var of Ident.t
  | Tty_arrow of ty * ty
  | Tty_prod of ty * ty * ty list
  | Tty_constr of ty list * string

let ty_int = Tty_constr ([], "int")
let ty_bool = Tty_constr ([], "bool")
let ty_unit = Tty_constr ([], "unit")
let tty_var ident = Tty_var ident
let tty_arrow a b = Tty_arrow (a, b)
let tty_prod a b xs = Tty_prod (a, b, xs)
let tty_constr params name = Tty_constr (params, name)

module VarSet = struct
  include Stdlib.Set.Make (Ident)
end

module Scheme = struct
  type t = Scheme of VarSet.t * ty
end

type type_kind =
  | Tty_abstract of ty option
  | Tty_variants of (Ident.t * ty option) list

type type_declaration =
  { tty_ident : Ident.t
  ; tty_params : Ident.t list
  ; tty_kind : type_kind
  }

type constructor_info =
  { constr_ident : Ident.t
  ; constr_type_ident : Ident.t
  ; constr_arg : ty option
  }

type value_binding =
  { tvb_pat : Parsetree.pattern
  ; tvb_body : Parsetree.expression
  ; tvb_scheme : Scheme.t
  }

type value_declaration = Parsetree.rec_flag * value_binding * value_binding list

type structure_item =
  | Tstr_value of value_declaration
  | Tstr_type of type_declaration

type structure = structure_item * structure_item list

open Base
open Base.Format
open Pprint
open Pprint.Parens

let rec show_ty ?(ctx = Parens.Free) = function
  | Tty_var ident -> sprintf "'ty%d" ident.id
  | Tty_arrow (a, b) ->
    set_parens ~ctx [ LeftSideArrow; App; Tuple ]
    @@ sprintf "%s -> %s" (show_ty ~ctx:LeftSideArrow a) (show_ty ~ctx:RightSideArrow b)
  | Tty_prod (a, b, xs) ->
    set_parens ~ctx [ App; Tuple ] @@ show_many ~sep:" * " show_ty (a :: b :: xs)
  | Tty_constr ([], name) -> name
  | Tty_constr ([ ty ], name) ->
    set_parens ~ctx [ App ] @@ sprintf "%s %s" (show_ty ~ctx:App ty) name
  | Tty_constr (ty1 :: ty2 :: tys, name) ->
    set_parens ~ctx [ App ]
    @@ sprintf "(%s) %s" (show_tuple (show_ty ~ctx:Free) (ty1, ty2, tys)) name
;;

let show_ty = show_ty ~ctx:Free
let pp_ty ppf ty = Format.fprintf ppf "%s" (show_ty ty)

let show_structure_item = function
  | Tstr_value (rec_flag, vb, vbs) ->
    let aux ~keyword { tvb_pat; tvb_body; tvb_scheme } =
      let (Scheme.Scheme (_vs, ty)) = tvb_scheme in
      sprintf
        "%s%s%s: %s = %s@."
        keyword
        (Parsetree.show_rec_flag rec_flag)
        (Parsetree.show_pattern tvb_pat)
        (show_ty ty)
        (Parsetree.show_expression tvb_body)
    in
    String.concat (aux ~keyword:"let" vb :: List.map ~f:(aux ~keyword:"and") vbs)
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
