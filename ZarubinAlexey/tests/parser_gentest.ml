(* tests/parser_gentest.ml *)

open QCheck
module G = QCheck.Gen
open Lambda_lib

open Ast

(* -------- генератор простых выражений AST -------- *)

let gen_var_name : string G.t =
  G.oneofl [ "x"; "y"; "z"; "n"; "m" ]

(* только Int, Var и простые бинарные операции *)
let rec gen_expr_sized (size : int) : Ast.name Ast.t G.t =
  let size = if size < 0 then 0 else size in
  if size = 0 then
    G.oneof
      [ G.map (fun n -> Int n) G.small_int
      ; G.map (fun v -> Var v) gen_var_name
      ]
  else
    let smaller () = gen_expr_sized (size - 1) in
    let gen_int = G.map (fun n -> Int n) G.small_int in
    let gen_var = G.map (fun v -> Var v) gen_var_name in
    let gen_op = G.oneofl [ Add; Sub; Mul; Div; Eq; Lt; Gt ] in
    let gen_binop =
      G.map3
        (fun op l r -> Binop (op, l, r))
        gen_op (smaller ()) (smaller ())
    in
    G.frequency
      [ 3, gen_int
      ; 3, gen_var
      ; 4, gen_binop
      ]

let gen_expr : Ast.name Ast.t G.t =
  G.sized (fun n -> gen_expr_sized (min 3 n))

let show_ast (e : Ast.name Ast.t) : string =
  Print.print_ast e

let arb_expr = make ~print:show_ast gen_expr

(* -------- свойство QuickCheck для принтера -------- *)

let print_ast_nonempty =
  Test.make
    ~name:"Print.print_ast возвращает непустую строку"
    ~count:100
    arb_expr
    (fun e ->
       let s = Print.print_ast e in
       String.length s > 0)

let () =
  QCheck_runner.run_tests_main [ print_ast_nonempty ]