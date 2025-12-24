open Lambda_lib
open QCheck

let oneof_list_gen xs =
  let open Gen in
  match xs with
  | [] -> failwith "oneof_list: empty list"
  | _ ->
    int_bound (List.length xs - 1) >>= fun i ->
    return (List.nth xs i)
;;

let oneof_weighted_gen xs =
  let open Gen in
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 xs in
  if total <= 0
  then failwith "oneof_weighted: empty list"
  else (
    int_bound (total - 1) >>= fun pick ->
    let rec select n = function
      | [] -> failwith "oneof_weighted: empty list"
      | (w, gen) :: rest -> if n < w then gen else select (n - w) rest
    in
    select pick xs)
;;

let gen_ident =
  oneof_list_gen [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "m"; "k"; "foo"; "bar"; "_x"; "x1"; "y'" ]
;;

let gen_rec_flag = oneof_list_gen [ Ast.Nonrec; Ast.Rec ]
let gen_unop = oneof_list_gen [ Ast.UMinus; Ast.UPlus ]
let gen_arith = oneof_list_gen [ Ast.Add; Ast.Sub; Ast.Mul; Ast.Div ]
let gen_cmp = oneof_list_gen [ Ast.Eq; Ast.Neq; Ast.Lt; Ast.Le; Ast.Gt; Ast.Ge ]

let gen_const =
  let open Gen in
  oneof_weighted_gen
    [ 6, map (fun n -> Ast.Int n) (int_bound 1000)
    ; 1, return (Ast.Unit ())
    ]
;;

let rec gen_expr size =
  let open Gen in
  let atom =
    oneof_weighted_gen
      [ 4, map (fun c -> Ast.Const c) gen_const
      ; 3, map (fun name -> Ast.Var name) gen_ident
      ]
  in
  if size = 0
  then atom
  else (
    let next = gen_expr (size / 2) in
    oneof_weighted_gen
      [ 5, atom
      ; 3, map2 (fun f x -> Ast.App (f, x)) next next
      ; 2, map2 (fun name body -> Ast.Lam (name, body)) gen_ident next
      ; 2, map4 (fun rf name rhs body -> Ast.Let (rf, name, rhs, body)) gen_rec_flag gen_ident next next
      ; 1, map2 (fun c t -> Ast.If (c, t, None)) next next
      ; 1, map3 (fun c t e -> Ast.If (c, t, Some e)) next next next
      ; 2, map2 (fun op e -> Ast.Unop (op, e)) gen_unop next
      ; 2, map3 (fun op l r -> Ast.BinopArithmetic (op, l, r)) gen_arith next next
      ; 2, map3 (fun op l r -> Ast.BinopComp (op, l, r)) gen_cmp next next
      ; 1, map (fun e -> Ast.Fix e) next
      ])
;;

let gen_toplevel size =
  let open Gen in
  oneof_weighted_gen
    [ 3, map (fun expr -> Ast.TExpr expr) (gen_expr size)
    ; 2, map3 (fun rf name rhs -> Ast.TLet (rf, name, rhs)) gen_rec_flag gen_ident (gen_expr size)
    ]
;;

let gen_program =
  let open Gen in
  sized (fun size -> list_size (0 -- 5) (gen_toplevel (max 1 (size / 2))))
;;

let arb_expr = make ~print:Ast.show_expr (Gen.sized gen_expr)
let arb_toplevel = make ~print:Ast.show_toplevel (Gen.sized gen_toplevel)
let arb_program = make ~print:Ast.show_program gen_program

let roundtrip_expr =
  Test.make ~name:"parser/printer roundtrip (expr)" ~count:300 arb_expr (fun expr ->
    let printed = Pprintast.string_of_expr expr in
    try Ast.equal_expr expr (Parser.expr_of_string printed) with
    | Parser.Error _ -> false)
;;

let roundtrip_toplevel =
  Test.make ~name:"parser/printer roundtrip (toplevel)" ~count:200 arb_toplevel (fun item ->
    let printed = Pprintast.string_of_toplevel item in
    try Ast.equal_toplevel item (Parser.toplevel_of_string printed) with
    | Parser.Error _ -> false)
;;

let roundtrip_program =
  Test.make ~name:"parser/printer roundtrip (program)" ~count:200 arb_program (fun program ->
    let printed = Pprintast.string_of_program program in
    try Ast.equal_program program (Parser.program_of_string printed) with
    | Parser.Error _ -> false)
;;

let () =
  QCheck_runner.run_tests
    ~verbose:true
    [ roundtrip_expr; roundtrip_toplevel; roundtrip_program ]
  |> Stdlib.exit
