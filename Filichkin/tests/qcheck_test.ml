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

let gen_constr_name =
  Gen.oneof
    [ pure "True"
    ; pure "False"
    ; pure "Some"
    ; pure "None"
    ; pure "Cons"
    ; pure "Nil"
    ; pure "A"
    ; pure "B"
    ]
;;

let gen_binop =
  Gen.(
    oneof
      [ pure Plus
      ; pure Minus
      ; pure Mult
      ; pure Div
      ; pure Equal
      ; pure NotEqual
      ; pure More
      ; pure Less
      ; pure EMore
      ; pure ELess
      ; pure And
      ; pure Or
      ])
;;

let rec gen_pattern size =
  if size <= 0
  then map (fun x -> PVar x) gen_var_name
  else
    frequency
      [ 3, map (fun x -> PVar x) gen_var_name
      ; 1, return PWildcard
      ; 1, map (fun n -> PConstr (n, [])) gen_constr_name
      ; ( 1
        , let* tuple_size = int_range 2 3 in
          map
            (fun ps -> PTuple ps)
            (list_size (return tuple_size) (gen_pattern (size / 2))) )
      ]
;;

let rec gen_expr size =
  let gen_param = Gen.map (fun v -> PVar v) gen_var_name in
  let gen_var = map (fun x -> Var x) gen_var_name in
  let gen_constr = map (fun x -> Constr x) gen_constr_name in
  let gen_int = map (fun i -> Int i) small_int in
  let gen_bool = map (fun b -> Bool b) bool in
  if size <= 1
  then oneof [ gen_var; gen_int; gen_bool; gen_constr ]
  else (
    let under_expr = gen_expr (size / 2) in
    let under_expr_small = gen_expr (size / 3) in
    let gen_binop =
      map3 (fun op x1 x2 -> BinOp (op, x1, x2)) gen_binop under_expr under_expr
    in
    let gen_if = map3 (fun c t e -> If (c, t, e)) under_expr under_expr under_expr in
    let gen_let =
      let gen_rec_flag = oneof [ return Rec; return NonRec ] in
      let gen_params = list_size (int_range 0 2) gen_param in
      let gen_bound =
        map2
          (fun params body ->
            List.fold_right (fun param acc -> Abs (param, acc)) params body)
          gen_params
          under_expr
      in
      let gen_body = map (fun x -> Some x) under_expr in
      map4
        (fun r v b s -> Let (r, PVar v, b, s))
        gen_rec_flag
        gen_var_name
        gen_bound
        gen_body
    in
    let gen_abs =
      let gen_params = list_size (int_range 1 2) gen_param in
      map2
        (fun params body ->
          List.fold_right (fun param acc -> Abs (param, acc)) params body)
        gen_params
        under_expr
    in
    let gen_app = map2 (fun x y -> App (x, y)) under_expr under_expr in
    let gen_unop =
      oneof
        [ map (fun e -> UnOp (Neg, e)) under_expr
        ; map (fun e -> UnOp (Not, e)) under_expr
        ]
    in
    let gen_tuple =
      let* tuple_size = int_range 2 3 in
      map (fun es -> Tuple es) (list_size (return tuple_size) under_expr)
    in
    let gen_match =
      let gen_case = pair (gen_pattern (size / 4)) under_expr_small in
      let cases = list_size (int_range 1 2) gen_case in
      map2 (fun scr cases -> Match (scr, cases)) under_expr_small cases
    in
    frequency
      [ 8, gen_int
      ; 7, gen_var
      ; 2, gen_bool
      ; 1, gen_constr
      ; 6, gen_binop
      ; 4, gen_if
      ; 3, gen_let
      ; 3, gen_abs
      ; 3, gen_app
      ; 2, gen_unop
      ; 2, gen_tuple
      ; 2, gen_match
      ])
;;

let rec shrink_expr = function
  | Int _ | Var _ | Bool _ | Constr _ -> Iter.empty
  | Tuple es ->
    let open Iter in
    Shrink.list ~shrink:shrink_expr es
    |> Iter.filter (fun es' -> List.length es' >= 2)
    |> Iter.map (fun es' -> Tuple es')
  | UnOp (op, e) ->
    let open Iter in
    of_list [ e ] <+> (shrink_expr e >|= fun e' -> UnOp (op, e'))
  | BinOp (op, x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> BinOp (op, e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> BinOp (op, x1, e2))
  | If (c, t, e) ->
    let open Iter in
    of_list [ c; t; e ]
    <+> (shrink_expr c >|= fun c' -> If (c', t, e))
    <+> (shrink_expr t >|= fun t' -> If (c, t', e))
    <+> (shrink_expr e >|= fun e' -> If (c, t, e'))
  | Let (rf, pat, bound, body_opt) ->
    let open Iter in
    (match body_opt with
     | None ->
       of_list [ bound ]
       <+> (shrink_expr bound >|= fun bound' -> Let (rf, pat, bound', None))
     | Some body ->
       of_list [ body; bound ]
       <+> (shrink_expr bound >|= fun bound' -> Let (rf, pat, bound', Some body))
       <+> (shrink_expr body >|= fun body' -> Let (rf, pat, bound, Some body')))
  | App (x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> App (e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> App (x1, e2))
  | Abs (x1, x2) ->
    let open Iter in
    of_list [ x2 ] <+> (shrink_expr x2 >|= fun e2 -> Abs (x1, e2))
  | Match (scrutinee, cases) ->
    let open Iter in
    of_list [ scrutinee ] <+> (shrink_expr scrutinee >|= fun s -> Match (s, cases))
;;

let arb_expr = QCheck.make ~print:print_expr ~shrink:shrink_expr (gen_expr 20)

let parse_after_print expr =
  let s = print_expr expr ^ " ;;" in
  match parser s with
  | Ok [ TLExpr expr' ] -> Ok expr'
  | Ok _ -> Error "expected expression"
  | Error (`parse_error msg) -> Error (Printf.sprintf "parse error on `%s`: %s" s msg)
;;

let print_parse_roundtrip =
  QCheck.Test.make
    ~count:1000
    ~name:"AST -> print_expr -> parser -> AST"
    arb_expr
    (fun expr ->
       match parse_after_print expr with
       | Ok expr' ->
         let original_ast_str = show_expr expr in
         let parsed_ast_str = show_expr expr' in
         if original_ast_str = parsed_ast_str
         then true
         else (
           Printf.eprintf "\nSOURCE:     %s\n" (print_expr expr);
           Printf.eprintf
             "ORIGINAL AST: %s\nPARSED AST:   %s\n"
             original_ast_str
             parsed_ast_str;
           false)
       | Error msg ->
         Printf.eprintf "\n%s\n" msg;
         false)
;;

let () = QCheck_runner.run_tests_main [ print_parse_roundtrip ]
