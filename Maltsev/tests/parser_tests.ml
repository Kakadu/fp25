open Lambda_lib
open QCheck

let ( >>= ) = Gen.( >>= )
let return = Gen.return

let gen_simple_expr =
  Gen.oneof
    [ return (Ast.Const 0)
    ; return (Ast.Const 1)
    ; return (Ast.Const (-1))
    ; return (Ast.Const 1212)
    ; return (Ast.Const 102002)
    ]
;;

let gen_ident = Gen.oneof [ return "x"; return "y"; return "z"; return "f"; return "g" ]

let gen_app_var =
  gen_ident
  >>= fun fname -> gen_simple_expr >>= fun arg -> return (Ast.App (Ast.Var (fname, arg)))
;;

let gen_app_abs =
  gen_ident
  >>= fun param ->
  Gen.oneof
    [ return (Ast.Ident param)
    ; gen_simple_expr
    ; Gen.map
        (fun r -> Ast.Binexpr (Ast.Plus, Ast.Ident param, Ast.Const r))
        (Gen.int_range 1 10)
    ]
  >>= fun body ->
  gen_simple_expr >>= fun arg -> return (Ast.App (Ast.Fun (param, body, arg)))
;;

let rec gen_app_mularg args =
  if args = 0
  then Gen.oneof [ gen_app_var; gen_app_abs ]
  else
    gen_app_mularg (args - 1)
    >>= fun inapp ->
    match inapp with
    | Ast.App x ->
      gen_simple_expr >>= fun arg -> return (Ast.App (Ast.Application (x, arg)))
    | _ -> gen_app_var
;;

let gen_app n =
  if n = 0
  then Gen.oneof [ gen_app_var; gen_app_abs ]
  else Gen.int_range 0 (n / 2) >>= fun m -> gen_app_mularg m
;;

let rec gen_expr n =
  match n with
  | 0 -> Gen.oneof [ gen_simple_expr; gen_app_var ]
  | 1 -> Gen.oneof [ gen_simple_expr; gen_app_var; gen_app_abs ]
  | _ ->
    Gen.int_range 0 (n / 2)
    >>= fun n1 ->
    Gen.oneof
      [ (gen_expr n1
         >>= fun e1 ->
         gen_expr n1
         >>= fun e2 ->
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
         >>= fun op -> return (Ast.Binexpr (op, e1, e2)))
      ; (gen_expr n1
         >>= fun cond ->
         gen_expr n1
         >>= fun tb -> gen_expr n1 >>= fun eb -> return (Ast.Ite (cond, tb, eb)))
      ; (gen_ident
         >>= fun arg -> gen_expr (n - 1) >>= fun body -> return (Ast.Abs (arg, body)))
      ; (Gen.int_range 0 n1 >>= fun n2 -> gen_app n2)
      ; (gen_ident
         >>= fun name ->
         gen_expr n1
         >>= fun bound ->
         gen_expr n1 >>= fun body -> return (Ast.Let (false, name, bound, body)))
      ; (gen_ident
         >>= fun name ->
         gen_ident
         >>= fun param ->
         gen_expr n1
         >>= fun fun_body ->
         let lambda = Ast.Abs (param, fun_body) in
         gen_expr n1 >>= fun body -> return (Ast.Let (true, name, lambda, body)))
      ]
;;

let arb_expr =
  let gen = Gen.sized gen_expr in
  let print = Some (fun expr -> Pprintast.pprint expr) in
  let shrink = Shrink.nil in
  QCheck.make gen ?print ~shrink
;;

let roundtrip =
  Test.make ~count:100 ~name:"roundtrip tests" arb_expr (fun expr ->
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
