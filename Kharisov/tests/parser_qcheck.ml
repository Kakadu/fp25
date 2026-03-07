[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open Interpret_lib

let gen_id = Gen.oneof_list [ "x"; "y"; "z"; "n"; "m"; "f"; "g" ]
let gen_binop = Gen.oneof_list Ast.[ Add; Sub; Mul; Div; Eq; Neq; Lt; Le; Gt; Ge ]
let gen_rec_flag = Gen.oneof_list Ast.[ Rec; NonRec ]

let rec gen_expr size =
  if size <= 0
  then
    Gen.oneof
      [ Gen.map (fun n -> Ast.EConst n) Gen.nat_small
      ; Gen.map (fun x -> Ast.EVar x) gen_id
      ]
  else (
    let smaller = gen_expr (size / 2) in
    let gen_let =
      Gen.map3
        (fun (rf, x) rhs body ->
          let rhs =
            match rf with
            | Ast.Rec ->
              (match rhs with
               | Ast.EFun _ -> rhs
               | _ -> Ast.EFun ("_k", rhs))
            | Ast.NonRec -> rhs
          in
          Ast.ELet (rf, x, rhs, body))
        (Gen.pair gen_rec_flag gen_id)
        smaller
        smaller
    in
    Gen.oneof_weighted
      [ 3, Gen.map (fun n -> Ast.EConst n) Gen.nat_small
      ; 3, Gen.map (fun x -> Ast.EVar x) gen_id
      ; 2, Gen.map3 (fun op l r -> Ast.EBinop (op, l, r)) gen_binop smaller smaller
      ; 1, Gen.map2 (fun x body -> Ast.EFun (x, body)) gen_id smaller
      ; 1, Gen.map2 (fun f a -> Ast.EApp (f, a)) smaller smaller
      ; 1, Gen.map3 (fun c t e -> Ast.EIf (c, t, e)) smaller smaller smaller
      ; 1, gen_let
      ])
;;

let arb_expr = make ~print:Pprint.to_string (Gen.sized (fun n -> gen_expr (min n 4)))

let round_trip_test =
  Test.make ~name:"print ∘ parse ∘ print = print" ~count:300 arb_expr (fun e ->
    let s = Pprint.to_string e in
    match Parser.parse s with
    | Error _ -> false
    | Ok e' -> String.equal (Pprint.to_string e') s)
;;

let nonempty_test =
  Test.make ~name:"print produces non-empty string" ~count:300 arb_expr (fun e ->
    String.length (Pprint.to_string e) > 0)
;;

let gen_invalid_program =
  Gen.oneof_list
    [ ""
    ; "let x ="
    ; "if x then"
    ; "let rec"
    ; "fun ->"
    ; "let = 1 in 2"
    ; "if then else"
    ; "+ +"
    ; "let x = in"
    ]
;;

let invalid_program_test =
  Test.make
    ~name:"invalid programs are rejected"
    ~count:100
    (make gen_invalid_program)
    (fun s ->
       match Parser.parse s with
       | Error _ -> true
       | Ok _ -> false)
;;

let no_crash_test =
  Test.make
    ~name:"parser never crashes on arbitrary input"
    ~count:500
    (make Gen.string)
    (fun s ->
       match Parser.parse s with
       | Ok _ | Error _ -> true)
;;

let idempotent_test =
  Test.make
    ~name:"idempotent: parse∘print∘parse∘print = parse∘print"
    ~count:300
    arb_expr
    (fun e ->
       let s1 = Pprint.to_string e in
       match Parser.parse s1 with
       | Error _ -> false
       | Ok e1 ->
         let s2 = Pprint.to_string e1 in
         (match Parser.parse s2 with
          | Error _ -> false
          | Ok e2 -> String.equal (Pprint.to_string e2) s2))
;;

let int_literal_test =
  Test.make
    ~name:"integer literals parse correctly"
    ~count:300
    (make ~print:string_of_int Gen.nat_small)
    (fun n ->
       let s = string_of_int n in
       match Parser.parse s with
       | Error _ -> false
       | Ok e -> String.equal (Pprint.to_string e) s)
;;

let var_literal_test =
  Test.make
    ~name:"variable names parse correctly"
    ~count:100
    (make ~print:Fun.id gen_id)
    (fun x ->
       match Parser.parse x with
       | Error _ -> false
       | Ok e -> String.equal (Pprint.to_string e) x)
;;

let neg_int_test =
  Test.make
    ~name:"negative integers round-trip exactly"
    ~count:300
    (make ~print:string_of_int (Gen.map (fun n -> -(n + 1)) Gen.nat_small))
    (fun n ->
      let s = Pprint.to_string (Ast.EConst n) in
      match Parser.parse s with
      | Error _ -> false
      | Ok e' -> String.equal (Pprint.to_string e') s)
;;

let () =
  QCheck_runner.run_tests_main
    [ round_trip_test
    ; nonempty_test
    ; invalid_program_test
    ; no_crash_test
    ; idempotent_test
    ; int_literal_test
    ; var_literal_test
    ; neg_int_test
    ]
;;
