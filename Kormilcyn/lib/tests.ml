[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** ***** UNIT TESTS COULD GO HERE (JUST AN EXAMPLE) *)
let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

(* These is a simple unit test that tests a single function 'fact'
   If you want to test something large, like interpretation of a piece
   of a minilanguge, it is not longer a unit tests but an integration test.
   Read about dune's cram tests and put the test into `demos/somefile.t`.
*)

open Miniml_lib
open Parser

let parse_optimistically str = Result.get_ok (parse str)
let pp = Printast.pp_named

(* Property-based tests (QuickCheck) *)
module Qc = struct
  open QCheck2
  open Ast

  let binop_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Leq -> "<="
    | Eq -> "="
    | Geq -> ">="
  ;;

  let rec to_src = function
    | Var v -> v
    | Int i -> string_of_int i
    | Neg e -> "(-" ^ to_src e ^ ")"
    | Bin (op, l, r) -> "(" ^ to_src l ^ " " ^ binop_to_string op ^ " " ^ to_src r ^ ")"
    | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ to_src e ^ ")"
    | App (f, a) -> "(" ^ to_src f ^ " " ^ to_src a ^ ")"
    | Let (x, e1, e2) -> "(let " ^ x ^ " = " ^ to_src e1 ^ " in " ^ to_src e2 ^ ")"
    | LetRec (f, e1, e2) -> "(let rec " ^ f ^ " = " ^ to_src e1 ^ " in " ^ to_src e2 ^ ")"
    | If (c, t, e) -> "(if " ^ to_src c ^ " then " ^ to_src t ^ " else " ^ to_src e ^ ")"
    | Fix -> "fix"
  ;;

  let rec equal a b =
    match a, b with
    | Var x, Var y -> String.equal x y
    | Int x, Int y -> x = y
    | Neg x, Neg y -> equal x y
    | Fun (x1, b1), Fun (x2, b2) -> String.equal x1 x2 && equal b1 b2
    | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
    | Bin (op1, l1, r1), Bin (op2, l2, r2) -> op1 = op2 && equal l1 l2 && equal r1 r2
    | Let (x1, e1, b1), Let (x2, e2, b2) ->
      String.equal x1 x2 && equal e1 e2 && equal b1 b2
    | LetRec (f1, e1, b1), LetRec (f2, e2, b2) ->
      String.equal f1 f2 && equal e1 e2 && equal b1 b2
    | If (c1, t1, e1), If (c2, t2, e2) -> equal c1 c2 && equal t1 t2 && equal e1 e2
    | Fix, Fix -> true
    | _ -> false
  ;;

  let var_gen =
    let names = [ "x"; "y"; "z"; "n"; "m"; "f" ] in
    Gen.oneofl names
  ;;

  let binop_gen = Gen.oneofl Ast.[ Add; Sub; Mul; Div; Leq; Eq; Geq ]

  let rec gen_ast depth =
    let open Gen in
    let base = [ map (fun v -> Var v) var_gen; map (fun i -> Int i) (int_bound 3) ] in
    if depth = 0
    then oneof base
    else
      oneof
        (base
         @ [ map2 (fun v b -> Fun (v, b)) var_gen (gen_ast (depth - 1))
           ; map2 (fun f a -> App (f, a)) (gen_ast (depth - 1)) (gen_ast (depth - 1))
           ; map3
               (fun op l r -> Bin (op, l, r))
               binop_gen
               (gen_ast (depth - 1))
               (gen_ast (depth - 1))
           ; map3
               (fun x e1 e2 -> Let (x, e1, e2))
               var_gen
               (gen_ast (depth - 1))
               (gen_ast (depth - 1))
           ; map4
               (fun f x body e2 -> LetRec (f, Fun (x, body), e2))
               var_gen
               var_gen
               (gen_ast (depth - 1))
               (gen_ast (depth - 1))
           ; map3
               (fun c t e -> If (c, t, e))
               (gen_ast (depth - 1))
               (gen_ast (depth - 1))
               (gen_ast (depth - 1))
           ; return Fix
           ])
  ;;

  let gen_ast_sized =
    let open Gen in
    sized (fun s -> gen_ast (3 + (s mod 3)))
  ;;

  let prop_parse_print_roundtrip =
    QCheck2.Test.make
      ~count:200
      ~name:"parser/pretty-printer roundtrip"
      ~print:to_src
      gen_ast_sized
      (fun ast ->
         match Parser.parse (to_src ast) with
         | Result.Ok ast' -> equal ast ast'
         | Result.Error _ -> false)
  ;;

  let%test_unit "parser/pretty-printer roundtrip" =
    QCheck2.Test.check_exn prop_parse_print_roundtrip
  ;;
end

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "x y");
  [%expect {| (App ((Var x), (Var y))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(x y)");
  [%expect {| (App ((Var x), (Var y))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun x -> x x)");
  [%expect {| (Fun (x, (App ((Var x), (Var x))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(fun f -> fun x -> f (x x))");
  [%expect {| (Fun (f, (Fun (x, (App ((Var f), (App ((Var x), (Var x))))))))) |}]
;;

let _ = Miniml_lib.Interpret.parse_and_run
let _ = Miniml_lib.Parser.parse_miniml
let _ = Miniml_lib.Printast.pp
let _ = Miniml_lib.Printast.show
