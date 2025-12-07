open Base
open QCheck
open DMotuzov_lib.Ast
open DMotuzov_lib.Prettyprinter
open DMotuzov_lib.Parser

let gen_ident =
  let open Gen in
  oneof [ pure "a"; pure "b"; pure "c"; pure "tmp"; pure "port" ]
;;

let gen_const =
  let open Gen in
  oneof [ small_int |> map (fun i -> Const_int i) ]
;;

let rec gen_expr size =
  let open Gen in
  let gen_var = gen_ident |> map (fun s -> Expr_var s) in
  let gen_const_expr = gen_const |> map (fun c -> Expr_const c) in
  if size <= 0
  then oneof [ gen_var; gen_const_expr ]
  else (
    let sub = gen_expr (size / 2) in
    let gen_bin =
      map3
        (fun op e1 e2 -> Expr_binary_op (op, e1, e2))
        (oneofl [ Plus; Sub; Mul; Div ])
        sub
        sub
    in
    let gen_if = map3 (fun c t f -> Expr_conditional (c, t, f)) sub sub sub in
    let gen_let = map3 (fun id e1 e2 -> Expr_let_in (id, e1, e2)) gen_ident sub sub in
    let gen_let_rec =
      map3 (fun id e1 e2 -> Expr_let_rec_in (id, e1, e2)) gen_ident sub sub
    in
    let gen_fun = map2 (fun id e -> Expr_fun (id, e)) gen_ident sub in
    let gen_app =
      map2 (fun f args -> Expr_ap (f, args)) sub (list_size (int_range 1 3) sub)
    in
    let gen_fix = sub |> map (fun e -> Expr_fix e) in
    frequency
      [ 4, gen_var
      ; 3, gen_const_expr
      ; 3, gen_bin
      ; 2, gen_if
      ; 2, gen_let
      ; 2, gen_let_rec
      ; 2, gen_fun
      ; 2, gen_app
      ; 1, gen_fix
      ])
;;

let show_expr expr = Stdlib.Format.asprintf "%a" pp expr
let arb_expr = make ~print:show_expr (Gen.sized gen_expr)
let wrap_for_parser expr_str = Printf.sprintf "let tmp = %s;;" expr_str

let parser_roundtrip_test =
  Test.make ~count:1000 ~name:"parser round-trip property" arb_expr (fun expr ->
    let code_string = wrap_for_parser (Stdlib.Format.asprintf "%a" pp expr) in
    match parse code_string with
    | Ok [ Top_let ("tmp", parsed_expr) ] ->
      if Stdlib.( = ) expr parsed_expr
      then true
      else
        Test.fail_reportf
          "AST mismatch after round-trip.\n\
           ===== Generated Source Code =====\n\
           %s\n\
           ===== Pretty-printed Input Expr =====\n\
           %s\n\
           ===== Original AST =====\n\
           %s\n\
           ===== Parsed AST =====\n\
           %s\n"
          code_string
          (show_expr expr)
          (show_expression expr)
          (show_expression parsed_expr)
    | Ok _ -> Test.fail_reportf "Parser returned unexpected toplevel structure"
    | Error err ->
      Test.fail_reportf
        "Parsing failed.\n\
         ===== Original AST =====\n\
         %s\n\
         ===== Code =====\n\
         %s\n\
         ===== Error =====\n\
         %s\n"
        (show_expression expr)
        code_string
        err)
;;

let () = QCheck_runner.run_tests_main [ parser_roundtrip_test ]
