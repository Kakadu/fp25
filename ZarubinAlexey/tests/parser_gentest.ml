(* tests/parser_gentest.ml *)

open QCheck
module G = QCheck.Gen
open Lambda_lib
open Ast

(* Генератор имён переменных для QCheck:
   случайно выбирает одно имя из фиксированного списка (x, y, z, n, m),
   чтобы удобно и предсказуемо генерировать Var-узлы в случайных AST. *)
let gen_var_name : string G.t = G.oneofl [ "x"; "y"; "z"; "n"; "m" ]

(* Для size>0 генерируем более сложные выражения:
   smaller() генерирует подвыражения с уменьшенным размером (size-1), чтобы рекурсия была конечной;
   gen_op выбирает случайный бинарный оператор;
   gen_binop строит Binop(op, l, r) из оператора и двух подвыражений;
   frequency задаёт веса: чаще генерируем атомы (Int/Var), но достаточно часто и Binop,
   чтобы тесты покрывали составные случаи. *)
let rec gen_expr_sized (size : int) : Ast.name Ast.t G.t =
  let size = if size < 0 then 0 else size in
  if size = 0
  then G.oneof [ G.map (fun n -> Int n) G.small_int; G.map (fun v -> Var v) gen_var_name ]
  else (
    let smaller () = gen_expr_sized (size - 1) in
    let gen_int = G.map (fun n -> Int n) G.small_int in
    let gen_var = G.map (fun v -> Var v) gen_var_name in
    let gen_op = G.oneofl [ Add; Sub; Mul; Div; Eq; Lt; Gt ] in
    let gen_binop =
      G.map3 (fun op l r -> Binop (op, l, r)) gen_op (smaller ()) (smaller ())
    in
    G.frequency [ 3, gen_int; 3, gen_var; 4, gen_binop ])
;;

(* Главный генератор выражений: QCheck сам подаёт параметр размера [n],
   а мы ограничиваем глубину сверху (max 3), чтобы деревья не разрастались
   и тесты оставались быстрыми и стабильными. *)
let gen_expr : Ast.name Ast.t G.t = G.sized (fun n -> gen_expr_sized (min 3 n))
let show_ast (e : Ast.name Ast.t) : string = Print.print_ast e

(* Арбитрар для QCheck: генерируем случайные выражения через [gen_expr],
   а для сообщений об ошибке печатаем их через [show_ast]. *)
let arb_expr = make ~print:show_ast gen_expr

(* Property тест для принтера AST: на 100 случайных выражениях проверяем,
   что Print.print_ast не возвращает пустую строку (и не ломается на разных формах AST). *)
let print_ast_nonempty =
  Test.make
    ~name:"Print.print_ast возвращает непустую строку"
    ~count:100
    arb_expr
    (fun e ->
       let s = Print.print_ast e in
       String.length s > 0)
;;

let () = QCheck_runner.run_tests_main [ print_ast_nonempty ]
