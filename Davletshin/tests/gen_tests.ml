[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Ast
open QCheck

let gen_varname = Gen.oneof_list [ "a"; "b"; "c"; "d"; "e"; "f"; "h"; "g" ]
let gen_binop = Gen.(oneof_list [ Plus; Minus; Times; Divide; Eq; Neq; Lt; Gt; Le; Ge ])

let rec gen_ast depth =
  let open Gen in
  let base = [ map (fun v -> Var v) gen_varname; map (fun i -> Int i) nat ] in
  if depth = 0
  then oneof base
  else
    oneof
      (base
       @ [ map2 (fun x t -> Abs (x, t)) gen_varname (gen_ast (depth - 1))
         ; map2 (fun f g -> App (f, g)) (gen_ast (depth - 1)) (gen_ast (depth - 1))
         ; map3
             (fun op a b -> Binop (op, a, b))
             gen_binop
             (gen_ast (depth - 1))
             (gen_ast (depth - 1))
         ; map (fun e -> Unop (Neg, e)) (gen_ast (depth - 1))
         ; map3
             (fun c t e -> If (c, t, e))
             (gen_ast (depth - 1))
             (gen_ast (depth - 1))
             (gen_ast (depth - 1))
         ; map3
             (fun p e1 e2 -> Let (Nonrec, p, e1, e2))
             gen_varname
             (gen_ast (depth - 1))
             (gen_ast (depth - 1))
         ; map4
             (fun n x b e2 -> Let (Rec, n, Abs (x, b), e2))
             gen_varname
             gen_varname
             (gen_ast (depth - 1))
             (gen_ast (depth - 1))
         ; map (fun e -> Fix e) (gen_ast (depth - 1))
         ; map (fun e -> Print e) (gen_ast (depth - 1))
         ])
;;

let equal = Ast.equal String.equal
let pprint ast = Format.asprintf "%a" Pprintast.pp ast
let gen_ast_sized = Gen.sized (fun s -> gen_ast (3 + (s mod 3)))
let arb_ast = make ~print:pprint gen_ast_sized

let pprint_test =
  Test.make ~count:1000 ~name:"pprint& parse test" arb_ast (fun ast ->
    match Parser.parse (pprint ast) with
    | Result.Ok ast' -> equal ast ast'
    | Result.Error _ -> false)
;;

(*TODO: gen_varname can generate a keyword*)
let _ = QCheck_runner.run_tests [ pprint_test ]
