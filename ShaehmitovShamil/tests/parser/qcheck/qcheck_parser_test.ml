[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Parser
open Ast
open QCheck

let gen_pattern size =
  let open Gen in
  if size <= 0
  then
    oneof
      [ map (fun s -> PVar s) (string_size ~gen:(char_range 'a' 'z') (return 1))
      ; return PAny
      ; return PUnit
      ]
  else
    frequency
      [ 5, map (fun s -> PVar s) (string_size ~gen:(char_range 'a' 'z') (return 1))
      ; 2, return PAny
      ; 2, return PUnit
      ]
;;

let rec gen_expr size =
  let open Gen in
  let gen_var = map (fun s -> Var s) (string_size ~gen:(char_range 'a' 'z') (return 1)) in
  let gen_const_int = map (fun i -> Const (CInt i)) small_int in
  if size <= 0
  then oneof [ gen_var; gen_const_int ]
  else (
    let sub_gen = gen_expr (size / 2) in
    let sub_pat = gen_pattern (size / 2) in
    let gen_unop = map2 (fun op e -> UnOp (op, e)) (oneofl [ Neg ]) sub_gen in
    let gen_binop =
      map3
        (fun op e1 e2 -> BinOp (op, e1, e2))
        (oneofl [ Add; Sub; Mul; Div ])
        sub_gen
        sub_gen
    in
    let gen_if = map3 (fun c t f -> If (c, t, f)) sub_gen sub_gen sub_gen in
    let gen_app = map2 (fun f arg -> App (f, arg)) sub_gen sub_gen in
    let gen_let =
      map4
        (fun rf p e1 e2 -> Let (rf, p, e1, e2))
        (oneofl [ NonRec; Rec ])
        sub_pat
        sub_gen
        sub_gen
    in
    let gen_fun =
      map2 (fun ps e -> FunExpr (ps, e)) (list_size (int_range 1 3) sub_pat) sub_gen
    in
    frequency
      [ 5, gen_const_int
      ; 5, gen_var
      ; 3, gen_unop
      ; 3, gen_binop
      ; 2, gen_if
      ; 2, gen_app
      ; 2, gen_let
      ; 2, gen_fun
      ])
;;

let shrink_pattern = function
  | PVar _ | PAny | PUnit -> Iter.empty
;;

let rec shrink_expr = function
  | Const _ | Var _ -> Iter.empty
  | UnOp (_, e) ->
    let open Iter in
    return e <+> (shrink_expr e >|= fun e' -> UnOp (Neg, e'))
  | BinOp (_, e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> BinOp (Add, e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> BinOp (Add, e1, e2'))
  | If (c, t, f) ->
    let open Iter in
    of_list [ t; f ]
    <+> (shrink_expr c >|= fun c' -> If (c', t, f))
    <+> (shrink_expr t >|= fun t' -> If (c, t', f))
    <+> (shrink_expr f >|= fun f' -> If (c, t, f'))
  | App (e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> App (e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> App (e1, e2'))
  | Let (_, _, e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> Let (NonRec, PVar "x", e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> Let (NonRec, PVar "x", e1, e2'))
  | FunExpr (_, e) ->
    let open Iter in
    return e <+> (shrink_expr e >|= fun e' -> FunExpr ([ PVar "x" ], e'))
;;

let arb_expr = make ~print:show_expr ~shrink:shrink_expr (Gen.sized gen_expr)

let parser_test =
  Test.make ~count:1000 ~name:"parser round-trip property" arb_expr (fun expr ->
    let code_string = pretty_print_expr expr in
    match parse code_string with
    | Ok parsed_expr ->
      if Caml.( = ) expr parsed_expr
      then true
      else
        Test.fail_reportf
          "AST mismatch after round-trip.\n\
           Original AST:   %s\n\
           Generated Code: %s\n\
           Parsed AST:     %s"
          (show_expr expr)
          code_string
          (show_expr parsed_expr)
    | Error msg ->
      Test.fail_reportf
        "Parse failed on generated code.\n\
         Original AST:   %s\n\
         Code:           %s\n\
         Error:          %s"
        (show_expr expr)
        code_string
        msg)
;;

let () = QCheck_runner.run_tests_main [ parser_test ]
