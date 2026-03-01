(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let pp_constant ppf = function
  | Const_int n -> fprintf ppf "%d" n
  | Const_bool n -> fprintf ppf "%b" n
  | Const_unit -> fprintf ppf "()"
;;

let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "rec"
  | NonRecursive -> fprintf ppf ""
;;

let pp_binop ppf = function
  | Add -> fprintf ppf "+"
  | Mult -> fprintf ppf "*"
  | Sub -> fprintf ppf "-"
  | Div -> fprintf ppf "/"
  | Gt -> fprintf ppf ">"
  | Lt -> fprintf ppf "<"
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | Gte -> fprintf ppf ">="
  | Lte -> fprintf ppf "<="
;;

let pp_unop ppf = function
  | Negative -> fprintf ppf "-"
  | Positive -> fprintf ppf "+"
  | Not -> fprintf ppf "not "
;;

let rec pp_type_annot ppf = function
  | Type_int -> fprintf ppf "int"
  | Type_bool -> fprintf ppf "bool"
  | Type_unit -> fprintf ppf "unit"
  | Type_var a -> fprintf ppf "'%s" a
  | Type_arrow (ty1, ty2) -> fprintf ppf "(%a -> %a)" pp_type_annot ty1 pp_type_annot ty2
  | Type_option ty ->
    (match ty with
     | Type_int | Type_unit | Type_var _ -> fprintf ppf "%a option" pp_type_annot ty
     | _ -> fprintf ppf "(%a) option" pp_type_annot ty)
;;

let rec pp_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var a -> fprintf ppf "%s" a
  | Pat_constant c -> fprintf ppf "%a" pp_constant c
  | Pat_option x ->
    (match x with
     | Some x -> fprintf ppf "Some (%a)" pp_pattern x
     | None -> fprintf ppf "None")
  | Pat_constraint (ty, x) -> fprintf ppf "(%a : %a)" pp_pattern x pp_type_annot ty
;;

let rec pp_expression ppf = function
  | Expr_const c -> fprintf ppf "%a" pp_constant c
  | Expr_ident ident -> fprintf ppf "%s" ident
  | Expr_fun (p, e) -> fprintf ppf "(fun %a -> %a)" pp_pattern p pp_expression e
  | Expr_apply (e1, e2) -> fprintf ppf "(%a) (%a)" pp_expression e1 pp_expression e2
  | Expr_if (c, th, el) ->
    let ppifexpr_helper ppf e =
      match e with
      | Expr_ident _ | Expr_const _ -> fprintf ppf "%a" pp_expression e
      | _ -> fprintf ppf "(%a)" pp_expression e
    in
    let ppifexpr = function
      | None -> fprintf ppf "if (%a) then (%a)" ppifexpr_helper c ppifexpr_helper th
      | Some x ->
        fprintf
          ppf
          "if (%a) then (%a) else (%a)"
          ppifexpr_helper
          c
          ppifexpr_helper
          th
          ppifexpr_helper
          x
    in
    ppifexpr el
  | Expr_let (recf, vb, vbl, e) ->
    fprintf
      ppf
      "let %a %a in (%a)"
      pp_rec_flag
      recf
      (fun ppf () ->
        fprintf ppf "%a" pp_value_binding vb;
        List.iter (fun vb1 -> fprintf ppf " and %a" pp_value_binding vb1) vbl)
      ()
      pp_expression
      e
  | Expr_binop (op, e1, e2) ->
    fprintf ppf "(%a) %a (%a)" pp_expression e1 pp_binop op pp_expression e2
  | Expr_unop (op, e) -> fprintf ppf "%a(%a)" pp_unop op pp_expression e
  | Expr_function (case, casel) ->
    fprintf ppf "function\n";
    List.iter
      (fun { case_pat; case_expr } ->
        fprintf ppf "| %a -> (%a)\n" pp_pattern case_pat pp_expression case_expr)
      (case :: casel)
  | Expr_match (e, case, casel) ->
    fprintf ppf "match (%a) with\n" pp_expression e;
    List.iter
      (fun { case_pat; case_expr } ->
        fprintf ppf "| %a -> (%a)\n" pp_pattern case_pat pp_expression case_expr)
      (case :: casel)
  | Expr_option x ->
    (match x with
     | Some x -> fprintf ppf "Some (%a)" pp_expression x
     | None -> fprintf ppf "None")
  | Expr_constraint (ty, x) -> fprintf ppf "(%a : %a)" pp_expression x pp_type_annot ty

and pp_value_binding ppf { vb_pat; vb_expr } =
  fprintf ppf "%a = %a" pp_pattern vb_pat pp_expression vb_expr
;;

let pp_structure_item ppf = function
  | Str_eval expr -> fprintf ppf "%a" pp_expression expr
  | Str_value (recf, vb, vbl) ->
    fprintf
      ppf
      "let %a %a"
      pp_rec_flag
      recf
      (fun ppf () ->
        fprintf ppf "%a" pp_value_binding vb;
        List.iter (fun vb1 -> fprintf ppf " and %a" pp_value_binding vb1) vbl)
      ()
;;

let pp_structure ppf structure =
  List.iter (fun item -> fprintf ppf "%a\n;;\n\n" pp_structure_item item) structure
;;
