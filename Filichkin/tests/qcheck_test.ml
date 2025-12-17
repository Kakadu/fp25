(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Filichkin_lib.Ast
open Filichkin_lib.Parser
open Filichkin_lib.Print
open Gen

let gen_var_name =
  Gen.(
    oneof
      [ pure "x"
      ; pure "y"
      ; pure "z"
      ; pure "a"
      ; pure "b"
      ; pure "c"
      ; pure "f"
      ; pure "g"
      ; pure "n"
      ; pure "m"
      ; pure "x42"
      ; pure "var1"
      ; pure "myVar"
      ])
;;

let gen_binop =
  Gen.(
    oneof
      [ pure Plus
      ; pure Minus
      ; pure Mult
      ; pure Div
      ; pure Equal
      ; pure More
      ; pure Less
      ; pure EMore
      ; pure ELess
      ])
;;

let rec gen_expr size =
  let gen_var = map (fun x -> Var x) gen_var_name in
  let gen_int = map (fun i -> Int i) small_int in
  if size < 1
  then oneof [ gen_var; gen_int ]
  else (
    let under_expr = gen_expr (size / 2) in
    let gen_binop =
      map3 (fun op x1 x2 -> BinOp (op, x1, x2)) gen_binop under_expr under_expr
    in
    let gen_if =
      let gen_else = oneof [ return None; map (fun x -> Some x) under_expr ] in
      map3 (fun c t e -> If (c, t, e)) under_expr under_expr gen_else
    in
    let gen_let =
      let gen_rec_flag = oneof [ return Rec; return NonRec ] in
      let gen_params = list_size (int_range 0 3) gen_var_name in
      let gen_bound =
        map2
          (fun params body ->
            List.fold_right (fun param acc -> Abs (param, acc)) params body)
          gen_params
          under_expr
      in
      let gen_some = oneof [ return None; map (fun x -> Some x) under_expr ] in
      map4 (fun r v b s -> Let (r, v, b, s)) gen_rec_flag gen_var_name gen_bound gen_some
    in
    let gen_abs =
      let gen_params = list_size (int_range 1 3) gen_var_name in
      map2
        (fun params body ->
          List.fold_right (fun param acc -> Abs (param, acc)) params body)
        gen_params
        under_expr
    in
    let gen_app = map2 (fun x y -> App (x, y)) under_expr under_expr in
    frequency
      [ 8, gen_int
      ; 7, gen_var
      ; 6, gen_binop
      ; 4, gen_if
      ; 3, gen_let
      ; 3, gen_abs
      ; 2, gen_app
      ])
;;

let rec shrink_expr = function
  | Int _ | Var _ -> Iter.empty
  | BinOp (op, x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> BinOp (op, e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> BinOp (op, x1, e2))
  | If (c, t, e) ->
    let open Iter in
    (match e with
     | None -> of_list [ t ] <+> (shrink_expr t >|= fun t' -> If (c, t', None))
     | Some e ->
       of_list [ c; t; e ]
       <+> (shrink_expr c >|= fun c' -> If (c', t, Some e))
       <+> (shrink_expr t >|= fun t' -> If (c, t', Some e))
       <+> (shrink_expr e >|= fun e1 -> If (c, t, Some e1)))
  | Let (rf, name, bound, body_opt) ->
    let open Iter in
    (match body_opt with
     | None ->
       of_list [ bound ]
       <+> (shrink_expr bound >|= fun bound' -> Let (rf, name, bound', None))
     | Some body ->
       of_list [ body; bound ]
       <+> (shrink_expr bound >|= fun bound' -> Let (rf, name, bound', Some body))
       <+> (shrink_expr body >|= fun body' -> Let (rf, name, bound, Some body')))
  | App (x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> App (e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> App (x1, e2))
  | Abs (x1, x2) ->
    let open Iter in
    of_list [ x2 ] <+> (shrink_expr x2 >|= fun e2 -> Abs (x1, e2))
;;

let arb_expr = QCheck.make ~print:print_expr ~shrink:shrink_expr (gen_expr 20)

let parse_after_print expr =
  let s = print_expr expr in
  match parser s with
  | Ok e -> Ok e
  | Error (`parse_error msg) -> Error (Printf.sprintf "parse error on `%s`: %s" s msg)
;;

let print_parse_roundtrip =
  QCheck.Test.make
    ~count:10000
    ~name:"AST -> print_expr -> parser -> AST"
    arb_expr
    (fun expr ->
       match parse_after_print expr with
       | Ok expr' ->
         let original_ast_str = print_ast expr in
         let parsed_ast_str = print_ast expr' in
         if original_ast_str = parsed_ast_str
         then true
         else (
           Printf.eprintf
             "\nORIGINAL STR: %s\nPARSED STR:   %s\n"
             (print_expr expr)
             (print_expr expr');
           Printf.eprintf
             "\nORIGINAL AST: %s\nPARSED AST:   %s\n"
             original_ast_str
             parsed_ast_str;
           false)
       | Error msg ->
         Printf.eprintf "\n%s\n" msg;
         false)
;;

let () = QCheck_runner.run_tests_main [ print_parse_roundtrip ]
