(** QuickCheck: print/parse roundtrip для программ *)

open QCheck
open ZarubinAlexey_lib.Ast
open ZarubinAlexey_lib.Parser
open ZarubinAlexey_lib.Print
open Gen

(* Набор допустимых имён переменных *)
let gen_var_name =
  oneofl [ "x"; "y"; "z"; "f"; "g"; "n"; "m" ]

(* Рекурсивный генератор выражений AST с ограничением размера *)
let rec gen_expr_sized size : Ast.name Ast.t Gen.t =
  if size <= 0 then
    (* На "листьях" только int и Var *)
    oneof
      [ map (fun n -> Int n) small_int
      ; map (fun v -> Var v) gen_var_name
      ]
  else
    let smaller () = gen_expr_sized (size / 2) in
    let gen_int = map (fun n -> Int n) small_int in
    let gen_var = map (fun v -> Var v) gen_var_name in
    let gen_abs =
      map2 (fun v body -> Abs (v, body)) gen_var_name (smaller ())
    in
    let gen_app =
      map2 (fun f a -> App (f, a)) (smaller ()) (smaller ())
    in
    let gen_binop =
      let gen_op = oneofl [ Add; Sub; Mul; Div; Eq; Lt; Gt ] in
      map3
        (fun op l r -> Binop (op, l, r))
        gen_op (smaller ()) (smaller ())
    in
    let gen_if =
      map3
        (fun c t e -> If (c, t, e))
        (smaller ()) (smaller ()) (smaller ())
    in
    let gen_let =
      map3
        (fun v e1 e2 -> Let (v, e1, e2))
        gen_var_name (smaller ()) (smaller ())
    in
    let gen_let_rec =
      map4
        (fun f x body in_e -> Let_rec (f, x, body, in_e))
        gen_var_name gen_var_name (smaller ()) (smaller ())
    in
    let gen_fix =
      map (fun body -> Fix body) (smaller ())
    in
    frequency
      [ 3, gen_int
      ; 3, gen_var
      ; 2, gen_abs
      ; 3, gen_app
      ; 3, gen_binop
      ; 2, gen_if
      ; 2, gen_let
      ; 2, gen_let_rec
      ; 1, gen_fix
      ]

let gen_expr = sized gen_expr_sized

(* Печать AST для отладочной информации в случае падения теста *)
let show_ast (e : Ast.name Ast.t) =
  (* здесь предполагается, что у тебя есть Print.print_ast или аналог.
     Если нет — можно сделать через Format.asprintf "%a" Print.pp_named *)
  Print.print_ast e

let arb_expr =
  make ~print:show_ast gen_expr

let print_parse_roundtrip =
  Test.make
    ~name:"print_expr / parse roundtrip"
    ~count:500
    arb_expr
    (fun expr ->
       let program = Print.print_expr expr in
       match Parser.parse program with
       | Ok expr' ->
         if expr = expr' then
           true
         else (
           (* дополнительный вывод в случае несоответствия *)
           Printf.eprintf "ORIGINAL AST: %s\n" (show_ast expr);
           Printf.eprintf
             "PARSED   AST: %s\n"
             (show_ast expr');
           false)
       | Error _ ->
         (* тоже считаем провалом: принтер должен выдавать синтаксически валидные программы *)
         Printf.eprintf
           "Parser failed on printed program:\n%s\n"
           program;
         false)

let () = QCheck_runner.run_tests_main [ print_parse_roundtrip ]