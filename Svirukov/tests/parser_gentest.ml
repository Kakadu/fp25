(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Lambda_lib.Parser
open Lambda_lib.Print
open Lambda_lib.Ast
open Gen

let gen_constant = Gen.(map (fun i -> CInt i) small_int)

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
      ; pure "h"
      ; pure "n"
      ; pure "m"
      ; pure "p"
      ; pure "q"
      ])
;;

let gen_pattern = Gen.map (fun name -> PVar name) gen_var_name

let gen_binop =
  Gen.(
    oneof
      [ pure Plus
      ; pure Minus
      ; pure Asteriks
      ; pure Dash
      ; pure Equals
      ; pure MoreThan
      ; pure LessThan
      ; pure EqLess
      ; pure EqMore
      ])
;;

let gen_expr =
  let rec aux depth =
    let open Gen in
    if depth <= 0
    then
      oneof [ map (fun c -> Constant c) gen_constant; map (fun p -> Var p) gen_pattern ]
    else (
      let subexpr = aux (depth - 1) in
      let medium_expr = aux (depth / 2) in
      let simple_expr = aux 0 in
      let gen_binopr_expr binop_depth =
        let rec gen_binop_atom bdepth =
          if bdepth <= 0
          then
            oneof
              [ map (fun c -> Constant c) gen_constant; map (fun p -> Var p) gen_pattern ]
          else (
            let sub_binop = gen_binop_atom (bdepth - 1) in
            oneof
              [ map (fun c -> Constant c) gen_constant
              ; map (fun p -> Var p) gen_pattern
              ; map
                  (fun (left, op, right) -> Binop (op, left, right))
                  (triple sub_binop gen_binop sub_binop)
              ])
        in
        gen_binop_atom binop_depth
      in
      oneof
        [ map (fun c -> Constant c) gen_constant
        ; map (fun p -> Var p) gen_pattern
        ; (let* left = oneof [ map (fun p -> Var p) gen_pattern ] in
           let* right = medium_expr in
           return (App (left, right)))
        ; (let* func =
             map (fun (pat, body) -> Func (pat, body)) (pair gen_pattern subexpr)
           in
           let* arg = medium_expr in
           return (App (func, arg)))
        ; (let* func = oneof [ map (fun p -> Var p) gen_pattern ] in
           let* arg1 = simple_expr in
           let* arg2 = simple_expr in
           return (App (App (func, arg1), arg2)))
        ; map (fun (pat, body) -> Func (pat, body)) (pair gen_pattern subexpr)
        ; (let* arg1 = gen_pattern in
           let* arg2 = gen_pattern in
           let* body = subexpr in
           return (Func (arg1, Func (arg2, body))))
        ; map
            (fun (left, op, right) -> Binop (op, left, right))
            (triple (gen_binopr_expr (depth / 2)) gen_binop (gen_binopr_expr (depth / 2)))
        ; (let* cond = gen_binopr_expr (depth / 2) in
           let* then_expr = subexpr in
           let* else_opt = Gen.option subexpr in
           return (Conditional (cond, then_expr, else_opt)))
        ; (let* rec_flag = oneof [ pure NonRec; pure Rec ] in
           let* pat = gen_pattern in
           let* e1 = medium_expr in
           let* next_opt = Gen.option subexpr in
           return (Let (rec_flag, pat, e1, next_opt)))
        ])
  in
  Gen.sized (fun size ->
    let depth = 1 + (size mod 6) in
    aux depth)
;;

let arb_program = make Gen.(map print_ast gen_expr)

let gen_invalid_program =
  make
    (oneof
       [ map (fun (n1, n2) -> Printf.sprintf "%d %d" n1 n2) (pair small_int small_int)
       ; map (fun n -> Printf.sprintf "%d + + %d" n n) small_int
       ; map (fun n -> Printf.sprintf "%d * * %d" n n) small_int
       ; pure "let x = "
       ; pure "if x > 5 then"
       ; pure "fun x ->"
       ; pure "x +"
       ; map (fun n -> Printf.sprintf "(%d + %d" n n) small_int
       ; map (fun n -> Printf.sprintf "((%d)))" n) small_int
       ; map
           (fun kw -> Printf.sprintf "let %s = 5 in x" kw)
           (oneof
              [ pure "let"
              ; pure "in"
              ; pure "fun"
              ; pure "rec"
              ; pure "if"
              ; pure "then"
              ; pure "else"
              ; pure "true"
              ; pure "false"
              ; pure "rec"
              ])
       ; map (fun n -> Printf.sprintf "let %d = 5 in x" n) small_int
       ; pure "()"
       ; pure "fun x .. -> x"
       ; pure "fun x y -> -> x + y"
       ; pure "let let x = 5 in x"
       ; pure "if if x then 1 else 0"
       ])
;;

let invalid_prog =
  Test.make
    ~name:"Invalid programs return errors"
    ~count:1000
    gen_invalid_program
    (fun program_str ->
       match parse program_str with
       | Ok r ->
         let _ = Printf.printf "\n%s %s\n" program_str (print_ast r) in
         false
       | Error _ -> true)
;;

let round_trip_property =
  Test.make ~name:"Parser round-trip property" ~count:1000 arb_program (fun program_str ->
    match parse program_str with
    | Ok ast ->
      let printed = print_ast ast in
      (match parse printed with
       | Ok ast2 -> ast = ast2
       | Error _ -> false)
    | Error _ -> true)
;;

let parsing_crash =
  Test.make
    ~name:"Check whether parsing crashes when it shouldn't"
    ~count:1000
    arb_program
    (fun program_str ->
       match parse program_str with
       | Ok _ -> true
       | Error _ -> false)
;;

QCheck_runner.run_tests ~verbose:true [ round_trip_property; parsing_crash; invalid_prog ]
