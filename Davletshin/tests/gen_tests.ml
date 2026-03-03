[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Ast
open QCheck2

let is_first_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_valid_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_varname str =
  String.length str > 0 && is_first_letter str.[0] && String.for_all is_valid_char str
;;

let gen_varname = Gen.oneofl [ "a"; "b"; "c"; "d"; "e"; "f"; "h"; "g" ]
let gen_binop = Gen.(oneofl [ Plus; Minus; Times; Divide; Eq; Neq; Lt; Gt; Le; Ge ])

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

let rec lol = function
  | Int n -> string_of_int n
  | Var s -> s
  | App (l, r) -> "(" ^ lol l ^ " " ^ lol r ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ lol t ^ ")"
  | Binop (op, l, r) ->
    let bop =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Divide -> "/"
      | Eq -> "="
      | Neq -> "<>"
      | Lt -> "<"
      | Gt -> ">"
      | Le -> "<="
      | Ge -> ">="
    in
    "(" ^ lol l ^ " " ^ bop ^ " " ^ lol r ^ ")"
  | Unop (op, e) ->
    let uop =
      match op with
      | Pos -> "+"
      | Neg -> "-"
    in
    "(" ^ uop ^ lol e ^ ")"
  | If (c, t, e) -> "(if " ^ lol c ^ " then " ^ lol t ^ " else " ^ lol e ^ ")"
  | Let (Nonrec, n, e1, e2) -> "(let " ^ n ^ " = " ^ lol e1 ^ " in " ^ lol e2 ^ ")"
  | Let (Rec, n, e1, e2) -> "(let rec " ^ n ^ " = " ^ lol e1 ^ " in " ^ lol e2 ^ ")"
  | Fix e -> "(fix " ^ lol e ^ ")"
  | Print e -> "(print " ^ lol e ^ ")"
;;

let rec equal e1 e2 =
  match e1, e2 with
  | Int x1, Int x2 -> x1 = x2
  | Var x1, Var x2 -> String.equal x1 x2
  | Abs (x1, t1), Abs (x2, t2) -> String.equal x1 x2 && equal t1 t2
  | App (f1, g1), App (f2, g2) -> equal f1 f2 && equal g1 g2
  | Binop (op1, a1, b1), Binop (op2, a2, b2) -> op1 = op2 && equal a1 a2 && equal b1 b2
  | Unop (op1, a1), Unop (op2, a2) -> op1 = op2 && equal a1 a2
  | If (c1, t1, e1), If (c2, t2, e2) -> equal c1 c2 && equal t1 t2 && equal e1 e2
  | Let (f1, p1, b1, e1), Let (f2, p2, b2, e2) ->
    f1 = f2 && String.equal p1 p2 && equal b1 b2 && equal e1 e2
  | Fix e1, Fix e2 -> equal e1 e2
  | Print e1, Print e2 -> equal e1 e2
  | _ -> false
;;

let pprint ast = Format.asprintf "%a" Pprintast.pp ast
let gen_ast_sized = Gen.sized (fun s -> gen_ast (3 + (s mod 3)))

let parse_test =
  Test.make ~count:200 ~name:"parser test" ~print:lol gen_ast_sized (fun ast ->
    match Parser.parse (lol ast) with
    | Result.Ok ast' -> equal ast ast'
    | Result.Error _ -> false)
;;

let pprint_test =
  Test.make ~count:200 ~name:"pprint test" ~print:pprint gen_ast_sized (fun ast ->
    match Parser.parse (pprint ast) with
    | Result.Ok ast' -> equal ast ast'
    | Result.Error _ -> false)
;;

(*TODO: gen_varname can generate a keyword*)
let _ = QCheck_runner.run_tests [ parse_test; pprint_test ]
