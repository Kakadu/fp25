(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Ast
open QCheck
open Gen

let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))

let rec gen_filtered_ident non_filtered =
  non_filtered
  >>= fun ident ->
  if is_keyword ident then gen_filtered_ident non_filtered else return ident
;;

let gen_some f = oneof [ return None; map (fun x -> Some x) f ]

let gen_ident =
  let non_filtered = string_size (int_range 1 8) ~gen:gen_char in
  gen_filtered_ident non_filtered
;;

let gen_constant =
  oneof
    [ map (fun c -> Const_int c) nat_small
    ; map (fun c -> Const_bool c) bool
    ; return Const_unit
    ]
;;

let gen_rec_flag = oneof_list [ Recursive; NonRecursive ]
let gen_binary_op = oneof_list [ Add; Mult; Sub; Div; Gt; Lt; Eq; Neq; Gte; Lte ]
let gen_unary_op = oneof_list [ Negative; Positive; Not ]

let rec gen_type_annot n =
  let gen_atom_type =
    oneof
      [ return Type_int
      ; return Type_bool
      ; return Type_unit
      ; map (fun t -> Type_var t) gen_ident
      ]
  in
  if n <= 0
  then gen_atom_type
  else (
    let gen_under_type = gen_type_annot (n / 2) in
    oneof_weighted
      [ 3, gen_atom_type
      ; 2, map (fun t -> Type_option t) gen_under_type
      ; 2, map2 (fun t1 t2 -> Type_arrow (t1, t2)) gen_under_type gen_under_type
      ])
;;

let rec gen_pattern n =
  let gen_atom_pattern =
    oneof
      [ map (fun x -> Pat_var x) gen_ident
      ; map (fun x -> Pat_constant x) gen_constant
      ; return Pat_any
      ]
  in
  if n <= 0
  then gen_atom_pattern
  else (
    let gen_under_pattern = gen_pattern (n / 2) in
    oneof_weighted
      [ 4, gen_atom_pattern
      ; 3, map (fun x -> Pat_option x) (gen_some gen_atom_pattern)
      ; ( 3
        , map2
            (fun t x -> Pat_constraint (t, x))
            (gen_type_annot (n / 2))
            gen_under_pattern )
      ])
;;

let rec gen_expression n =
  let gen_atom_expr =
    oneof
      [ map (fun c -> Expr_const c) gen_constant
      ; map (fun id -> Expr_ident id) gen_ident
      ]
  in
  if n <= 0
  then gen_atom_expr
  else (
    let gen_under_expr = gen_expression (n / 2) in
    oneof_weighted
      [ 5, gen_atom_expr
      ; 4, map (fun e -> Expr_option e) (gen_some gen_atom_expr)
      ; ( 4
        , map2 (fun t e -> Expr_constraint (t, e)) (gen_type_annot (n / 5)) gen_under_expr
        )
      ; ( 4
        , map3
            (fun op e1 e2 -> Expr_binop (op, e1, e2))
            gen_binary_op
            gen_under_expr
            gen_under_expr )
      ; 3, map2 (fun op e -> Expr_unop (op, e)) gen_unary_op gen_under_expr
      ; 3, map2 (fun p e -> Expr_fun (p, e)) (gen_pattern (n / 5)) gen_under_expr
      ; 3, map2 (fun e1 e2 -> Expr_apply (e1, e2)) gen_under_expr gen_under_expr
      ; ( 1
        , map3
            (fun c t e -> Expr_if (c, t, e))
            gen_under_expr
            gen_under_expr
            (gen_some gen_under_expr) )
      ; ( 1
        , map4
            (fun rf vb vb_l e -> Expr_let (rf, vb, vb_l, e))
            gen_rec_flag
            (gen_value_binding (n / 5))
            (list_size (0 -- 2) (gen_value_binding (n / 5)))
            gen_under_expr )
      ; ( 1
        , map2
            (fun cs cs_l -> Expr_function (cs, cs_l))
            (gen_case (n / 5))
            (list_size (0 -- 2) (gen_case (n / 5))) )
      ; ( 1
        , map3
            (fun e cs cs_l -> Expr_match (e, cs, cs_l))
            gen_under_expr
            (gen_case (n / 5))
            (list_size (0 -- 2) (gen_case (n / 5))) )
      ])

and gen_value_binding n =
  map2
    (fun vb_p vb_e -> { vb_pat = vb_p; vb_expr = vb_e })
    (gen_pattern (n / 2))
    (gen_expression n)

and gen_case n =
  map2
    (fun cs_p cs_e -> { case_pat = cs_p; case_expr = cs_e })
    (gen_pattern (n / 2))
    (gen_expression n)
;;

let gen_structure =
  list_size
    (1 -- 3)
    (oneof
       [ map (fun e -> Str_eval e) (gen_expression 20)
       ; map3
           (fun rf vb vb_l -> Str_value (rf, vb, vb_l))
           gen_rec_flag
           (gen_value_binding 10)
           (list_size (0 -- 1) (gen_value_binding 5))
       ])
;;
