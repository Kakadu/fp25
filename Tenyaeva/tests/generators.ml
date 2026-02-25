(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Ast
open QCheck
open Gen

let gen_atom =
  oneof
    [ map (fun c -> Expr_const c) gen_constant; map (fun id -> Expr_ident id) gen_ident ]
;;

let rec gen_expression n =
  if n = 0
  then gen_atom
  else (
    let gen_under_expr = gen_expression (n / 5) in
    let gen_some f = oneof [ return None; map (fun x -> Some x) f ] in
    oneof_weighted
      [ 5, gen_atom
      ; 4, map (fun e -> Expr_option e) (gen_some gen_atom)
      ; 4, map2 (fun t e -> Expr_constraint (t, e)) gen_type_annot gen_atom
      ; ( 4
        , map3
            (fun op e1 e2 -> Expr_binop (op, e1, e2))
            gen_binary_op
            gen_under_expr
            gen_under_expr )
      ; 3, map2 (fun op e -> Expr_unop (op, e)) gen_unary_op gen_under_expr
      ; 3, map2 (fun p e -> Expr_fun (p, e)) (gen_pattern_sized (n / 20)) gen_under_expr
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
    gen_pattern
    (gen_expression (n / 20))

and gen_case n =
  map2
    (fun cs_p cs_e -> { case_pat = cs_p; case_expr = cs_e })
    gen_pattern
    (gen_expression (n / 20))
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
