open Lambda_lib
open QCheck

let print_expr expr = Format.asprintf "%a" Printer.pp expr

let oneof_values values = Gen.oneof (List.map Gen.return values)

let gen_ident =
  oneof_values
    [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "m"; "_x"; "foo"; "bar"; "ifx"; "recx"; "funny" ]
;;

let gen_op = oneof_values [ Ast.Add; Sub; Mul; Div; Lt; Eq; Mt ]
let gen_rec_flag = oneof_values [ Ast.Val; Rec ]
let gen_const = Gen.map (fun n -> Ast.Const n) (Gen.int_bound 100)

let rec gen_expr size =
  let open Gen in
  if size <= 0
  then oneof [ gen_const; map (fun name -> Ast.Var name) gen_ident ]
  else (
    let sub = gen_expr (size / 2) in
    oneof
      [ oneof [ gen_const; map (fun name -> Ast.Var name) gen_ident ]
      ; map (fun e -> Ast.Neg e) sub
      ; map3 (fun op l r -> Ast.BinOp (op, l, r)) gen_op sub sub
      ; map3 (fun c t e -> Ast.If (c, t, e)) sub sub sub
      ; map2 (fun f a -> Ast.App (f, a)) sub sub
      ; map2 (fun arg body -> Ast.Fun (arg, body)) gen_ident sub
      ; map4
          (fun rec_flag name value body -> Ast.Let (rec_flag, name, value, body))
          gen_rec_flag
          gen_ident
          sub
          sub
      ])
;;

let arb_expr = make ~print:print_expr Gen.(sized gen_expr)

let roundtrip_test =
  Test.make ~count:300 ~name:"printer/parser roundtrip" arb_expr (fun expr ->
    let printed = print_expr expr in
    match Parser.parse printed with
    | Ok reparsed -> reparsed = expr
    | Error _ -> false)
;;

let parser_boundary_tests =
  [ Test.make ~count:1 ~name:"let recx stays a plain identifier" unit (fun () ->
      match Parser.parse "let recx = 1 in recx" with
      | Ok (Ast.Let (Ast.Val, "recx", Ast.Const 1, Ast.Var "recx")) -> true
      | _ -> false)
  ; Test.make ~count:1 ~name:"ifx parses as identifier" unit (fun () ->
      match Parser.parse "ifx" with
      | Ok (Ast.Var "ifx") -> true
      | _ -> false)
  ; Test.make ~count:1 ~name:"funny parses as identifier" unit (fun () ->
      match Parser.parse "funny" with
      | Ok (Ast.Var "funny") -> true
      | _ -> false)
  ; Test.make ~count:1 ~name:"invalid let is rejected" unit (fun () ->
      match Parser.parse "let x = in x" with
      | Error _ -> true
      | Ok _ -> false)
  ; Test.make ~count:1 ~name:"invalid if is rejected" unit (fun () ->
      match Parser.parse "if 1 then else 2" with
      | Error _ -> true
      | Ok _ -> false)
  ; Test.make ~count:1 ~name:"fun without parameter is rejected" unit (fun () ->
      match Parser.parse "fun -> x" with
      | Error _ -> true
      | Ok _ -> false)
  ]
;;

let () = QCheck_base_runner.run_tests_main (roundtrip_test :: parser_boundary_tests)
