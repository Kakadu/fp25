open Lambda_lib
open QCheck

let ( >>= ) = Gen.( >>= )
let return = Gen.return
let gen_var = Gen.oneof [ return (Ast.Ident "x"); return (Ast.Ident "X1") ]
let gen_int = Gen.oneof [ return (Ast.Const 8); return (Ast.Const (-13)) ]

let gen_binexpr e1 e2 =
  Gen.oneof
    [ return Ast.Plus
    ; return Ast.Minus
    ; return Ast.Mul
    ; return Ast.Div
    ; return Ast.Eq
    ; return Ast.Neq
    ; return Ast.Le
    ; return Ast.Bi
    ]
  >>= fun op -> return (Ast.Binexpr (op, e1, e2))
;;

let gen_ite e1 e2 e3 = return (Ast.Ite (e1, e2, e3))
let gen_abs e1 = gen_var >>= fun name -> return (Ast.Abs (name, e1))
let gen_app e1 = gen_var >>= fun name -> return (Ast.App (name, e1))

let gen_let_nonrec e1 e2 =
  gen_var
  >>= fun name ->
  gen_app e1 >>= fun letexpr -> return (Ast.Let (Ast.Recflag false, name, letexpr, e2))
;;

let gen_let_rec e1 e2 =
  gen_var
  >>= fun name ->
  gen_abs e1 >>= fun letexpr -> return (Ast.Let (Ast.Recflag true, name, letexpr, e2))
;;

let rec gen_expr n =
  match n with
  | 0 -> gen_int
  | 1 -> gen_var
  | _ ->
    Gen.oneof
      [ (Gen.int_range 0 (n - 1)
         >>= fun n1 ->
         gen_expr n1
         >>= fun e1 ->
         Gen.int_range 0 (n - 1) >>= fun n2 -> gen_expr n2 >>= fun e2 -> gen_binexpr e1 e2
        )
      ; (Gen.int_range 0 (n - 1)
         >>= fun n1 ->
         gen_expr n1
         >>= fun cond ->
         Gen.int_range 0 (n - 1)
         >>= fun n2 ->
         gen_expr n2
         >>= fun tb ->
         Gen.int_range 0 (n - 1)
         >>= fun n3 -> gen_expr n3 >>= fun eb -> gen_ite cond tb eb)
      ; (Gen.int_range 0 (n - 1) >>= fun n1 -> gen_expr n1 >>= fun e1 -> gen_abs e1)
      ; (Gen.int_range 0 (n - 1) >>= fun n1 -> gen_expr n1 >>= fun e1 -> gen_app e1)
      ; (Gen.int_range 0 (n - 1)
         >>= fun n1 ->
         gen_expr n1
         >>= fun e1 ->
         Gen.int_range 0 (n - 1)
         >>= fun n2 -> gen_expr n2 >>= fun e2 -> gen_let_nonrec e1 e2)
      ; (Gen.int_range 0 (n - 1)
         >>= fun n1 ->
         gen_expr n1
         >>= fun e1 ->
         Gen.int_range 0 (n - 1) >>= fun n2 -> gen_expr n2 >>= fun e2 -> gen_let_rec e1 e2
        )
      ]
;;

let genexpr_sized = Gen.sized gen_expr

let arb_expr =
  let gen = Gen.sized gen_expr in
  let print = Some (fun expr -> Pprintast.pprint expr) in
  let shrink = Shrink.nil in
  QCheck.make gen ?print ~shrink
;;

let roundtrip =
  Test.make ~count:1000 ~name:"roundtrip tests" arb_expr (fun expr ->
    let res = Parser.parse (Pprintast.pprint expr) in
    match res with
    | Ok x -> expr = x
    | Error _ -> false)
;;

let () =
  let result = QCheck_runner.run_tests [ roundtrip ] in
  if result = 0
  then Printf.printf "All tests passed!\n"
  else Printf.printf "Some tests failed\n";
  flush stdout
;;
