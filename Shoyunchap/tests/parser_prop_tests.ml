(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib
open Ast
open Parser
open Printer
open QCheck

let gen_name =
  let names = [ "x"; "y"; "z"; "n"; "m"; "f"; "g"; "h"; "a"; "b"; "c" ] in
  Gen.oneofl names
;;

let gen_const =
  Gen.frequency [ 4, Gen.map (fun n -> Int n) Gen.small_int; 1, Gen.return Unit ]
;;

let gen_op = Gen.oneofl [ OpAdd; OpSub; OpMul; OpDiv; OpEq; OpGt; OpLt; OpGte; OpLte ]

let rec gen_expr depth =
  let leaf =
    [ Gen.map (fun c -> Ast.Const c) gen_const; Gen.map (fun n -> Ast.Var n) gen_name ]
  in
  if depth <= 0
  then Gen.oneof leaf
  else (
    let sub = gen_expr (depth / 2) in
    let simpler = gen_expr (depth - 1) in
    let let_gen =
      let open Gen in
      oneofl [ NonRec; Rec ]
      >>= fun kind ->
      gen_name
      >>= fun name ->
      (match kind with
       | Rec -> map2 (fun arg body -> Ast.Fun (arg, body)) gen_name simpler
       | NonRec -> simpler)
      >>= fun rhs ->
      option sub
      >>= fun body_opt ->
      let scope =
        match body_opt with
        | None -> GlobalVar
        | Some _ -> LocalVar
      in
      return (Ast.Let (scope, kind, name, rhs, body_opt))
    in
    Gen.oneof
      (leaf
       @ [ Gen.map2 (fun n body -> Ast.Fun (n, body)) gen_name simpler
         ; Gen.map2 (fun fn arg -> Ast.App (fn, arg)) simpler simpler
         ; Gen.map3 (fun l op r -> Ast.BinOp (op, l, r)) simpler gen_op simpler
         ; Gen.map3 (fun c t e_opt -> Ast.If (c, t, e_opt)) sub simpler (Gen.option sub)
         ; let_gen
         ]))
;;

let arb_expr =
  make ~print:string_of_expr (Gen.sized (fun sz -> gen_expr (1 + (sz mod 6))))
;;

let invalid_programs =
  [ "let = x"
  ; "fun -> x"
  ; "if x then"
  ; "x + + 2"
  ; "let rec f ="
  ; "f ( ) )"
  ; "let 5 = 3"
  ; "fun x y -> -> x"
  ; "if (1 <) then 2"
  ]
;;

let prop_roundtrip =
  Test.make ~name:"parser/pretty-printer roundtrip" ~count:500 arb_expr (fun e ->
    let printed = string_of_expr e in
    match parse printed with
    | Ok e' -> e' = e
    | Error _ -> false)
;;

let prop_stable_string =
  Test.make ~name:"pretty-print is idempotent after parse" ~count:500 arb_expr (fun e ->
    let first = string_of_expr e in
    match parse first with
    | Error _ -> false
    | Ok e' ->
      let second = string_of_expr e' in
      let reparse = parse second in
      (match reparse with
       | Ok e'' -> second = string_of_expr e'' && e' = e''
       | Error _ -> false))
;;

let prop_invalid_fails =
  let gen_bad = Gen.oneofl invalid_programs in
  Test.make ~name:"invalid snippets are rejected" ~count:200 (make gen_bad) (fun src ->
    match parse src with
    | Ok _ -> false
    | Error _ -> true)
;;

let () =
  (* Keep progress lines but suppress frequent interim updates *)
  QCheck_base_runner.set_time_between_msg 1e9
;;

let () =
  QCheck_runner.run_tests_main
    ~argv:[| Sys.argv.(0); "-v"; "--colors" |]
    [ prop_roundtrip; prop_stable_string; prop_invalid_fails ]
;;
