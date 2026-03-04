(** Copyright 2025, XRenso *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Mml.Ast
open Mml.Parser
open Mml.Print
open Mml.Interpret

(* ==================== Generators ==================== *)

(** Generator for valid miniML identifiers *)
let gen_ident =
  QCheck.Gen.oneof
    [ QCheck.Gen.pure "x"
    ; QCheck.Gen.pure "y"
    ; QCheck.Gen.pure "z"
    ; QCheck.Gen.pure "a"
    ; QCheck.Gen.pure "b"
    ; QCheck.Gen.pure "n"
    ; QCheck.Gen.pure "m"
    ; QCheck.Gen.pure "f"
    ; QCheck.Gen.pure "g"
    ; QCheck.Gen.pure "foo"
    ; QCheck.Gen.pure "bar"
    ; QCheck.Gen.pure "x1"
    ; QCheck.Gen.pure "val_"
    ]
;;

(** Generator for binary operators *)
let gen_binop = QCheck.Gen.oneof_list [ Add; Sub; Mul; Div; Lt; Leq; Eq; Geq; Gt ]

(** Generator for random AST expressions with bounded depth *)
let rec gen_expr size =
  let open QCheck.Gen in
  if size <= 0
  then oneof [ map (fun i -> Int i) (int_range 0 100); map (fun v -> Var v) gen_ident ]
  else (
    let sub = gen_expr (size / 2) in
    let sub_small = gen_expr (size / 3) in
    oneof_weighted
      [ 3, map (fun i -> Int i) (int_range 0 100)
      ; 1, map (fun v -> Var v) gen_ident
      ; 2, map3 (fun op l r -> Bin (op, l, r)) gen_binop sub sub
      ; 1, map (fun e -> Neg e) sub
      ; 2, map2 (fun p body -> Fun (p, body)) gen_ident sub
      ; 2, map2 (fun f arg -> App (f, arg)) sub sub
      ; 2, map3 (fun name rhs body -> Let (name, rhs, body)) gen_ident sub sub
      ; 1, map3 (fun cond t e -> If (cond, t, e)) sub_small sub_small sub_small
      ; ( 1
        , let* fname = gen_ident in
          let* param = gen_ident in
          let* fn_body = sub in
          let* body = sub in
          return (LetRec (fname, Fun (param, fn_body), body)) )
      ])
;;

let arbitrary_expr =
  QCheck.make (QCheck.Gen.sized_size (QCheck.Gen.int_range 1 4) gen_expr) ~print:to_string
;;

(** Generator for closed arithmetic expressions (no free variables) *)
let rec gen_arith_expr size =
  let open QCheck.Gen in
  if size <= 0
  then map (fun i -> Int i) (int_range 0 50)
  else (
    let sub = gen_arith_expr (size / 2) in
    oneof_weighted
      [ 3, map (fun i -> Int i) (int_range 0 50)
      ; 2, map2 (fun l r -> Bin (Add, l, r)) sub sub
      ; 2, map2 (fun l r -> Bin (Sub, l, r)) sub sub
      ; 2, map2 (fun l r -> Bin (Mul, l, r)) sub sub
      ; 1, map (fun e -> Neg e) sub
      ])
;;

let arbitrary_arith =
  QCheck.make
    (QCheck.Gen.sized_size (QCheck.Gen.int_range 1 3) gen_arith_expr)
    ~print:to_string
;;

(** Structural equality for AST *)
let rec ast_equal a b =
  match a, b with
  | Int x, Int y -> x = y
  | Var x, Var y -> String.equal x y
  | Neg a, Neg b -> ast_equal a b
  | Fun (p1, b1), Fun (p2, b2) -> String.equal p1 p2 && ast_equal b1 b2
  | App (f1, a1), App (f2, a2) -> ast_equal f1 f2 && ast_equal a1 a2
  | Bin (op1, l1, r1), Bin (op2, l2, r2) ->
    op1 = op2 && ast_equal l1 l2 && ast_equal r1 r2
  | Let (n1, v1, b1), Let (n2, v2, b2) ->
    String.equal n1 n2 && ast_equal v1 v2 && ast_equal b1 b2
  | LetRec (n1, v1, b1), LetRec (n2, v2, b2) ->
    String.equal n1 n2 && ast_equal v1 v2 && ast_equal b1 b2
  | If (c1, t1, e1), If (c2, t2, e2) ->
    ast_equal c1 c2 && ast_equal t1 t2 && ast_equal e1 e2
  | _ -> false
;;

(* ==================== Parser & Printer tests ==================== *)

let no_output _ = ()

(** Roundtrip: parse(print(ast)) = ast *)
let test_roundtrip =
  QCheck.Test.make
    ~count:1000
    ~name:"parser/printer roundtrip: parse(print(ast)) = ast"
    arbitrary_expr
    (fun ast ->
       let printed = to_string ast in
       match parse printed with
       | Ok parsed -> ast_equal ast parsed
       | Error _ -> false)
;;

(** Printer is deterministic: print(ast) always gives the same result *)
let test_print_deterministic =
  QCheck.Test.make
    ~count:500
    ~name:"printer: deterministic output"
    arbitrary_expr
    (fun ast -> String.equal (to_string ast) (to_string ast))
;;

(** Printer always produces parseable output *)
let test_print_parseable =
  QCheck.Test.make
    ~count:1000
    ~name:"printer: output is always parseable"
    arbitrary_expr
    (fun ast ->
       let printed = to_string ast in
       match parse printed with
       | Ok _ -> true
       | Error _ -> false)
;;

(** Double roundtrip: print(parse(print(ast))) = print(ast) *)
let test_double_roundtrip =
  QCheck.Test.make
    ~count:500
    ~name:"parser/printer: double roundtrip"
    arbitrary_expr
    (fun ast ->
       let s1 = to_string ast in
       match parse s1 with
       | Ok ast2 ->
         let s2 = to_string ast2 in
         String.equal s1 s2
       | Error _ -> false)
;;

(* ==================== Interpreter tests ==================== *)

(** Reference evaluator for arithmetic *)
let rec eval_arith = function
  | Int n -> Some n
  | Neg e ->
    (match eval_arith e with
     | Some n -> Some (-n)
     | None -> None)
  | Bin (Add, l, r) ->
    (match eval_arith l, eval_arith r with
     | Some a, Some b -> Some (a + b)
     | _ -> None)
  | Bin (Sub, l, r) ->
    (match eval_arith l, eval_arith r with
     | Some a, Some b -> Some (a - b)
     | _ -> None)
  | Bin (Mul, l, r) ->
    (match eval_arith l, eval_arith r with
     | Some a, Some b -> Some (a * b)
     | _ -> None)
  | _ -> None
;;

(** Interpreter arithmetic matches reference *)
let test_interp_arith =
  QCheck.Test.make
    ~count:500
    ~name:"interpreter: arithmetic matches reference"
    arbitrary_arith
    (fun ast ->
       match eval_arith ast, run ~output:no_output 10000 ast with
       | Some expected, Ok actual -> expected = actual
       | None, _ -> true
       | _ -> false)
;;

(** Identity function applied to int returns that int *)
let test_interp_identity =
  QCheck.Test.make
    ~count:200
    ~name:"interpreter: identity fun returns argument"
    (QCheck.make QCheck.Gen.(int_range 0 1000) ~print:string_of_int)
    (fun n ->
      let expr = App (Fun ("x", Var "x"), Int n) in
      match run ~output:no_output 1000 expr with
      | Ok result -> result = n
      | Error _ -> false)
;;

(** Constant function ignores second argument *)
let test_interp_const =
  QCheck.Test.make
    ~count:200
    ~name:"interpreter: const fun ignores second arg"
    (QCheck.make
       QCheck.Gen.(pair (int_range 0 100) (int_range 0 100))
       ~print:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b))
    (fun (a, b) ->
      let expr = App (App (Fun ("x", Fun ("y", Var "x")), Int a), Int b) in
      match run ~output:no_output 1000 expr with
      | Ok result -> result = a
      | Error _ -> false)
;;

(** Let binding: let x = n in x evaluates to n *)
let test_interp_let_id =
  QCheck.Test.make
    ~count:200
    ~name:"interpreter: let x = n in x = n"
    (QCheck.make QCheck.Gen.(int_range 0 1000) ~print:string_of_int)
    (fun n ->
      let expr = Let ("x", Int n, Var "x") in
      match run ~output:no_output 1000 expr with
      | Ok result -> result = n
      | Error _ -> false)
;;

(** If with nonzero condition chooses then-branch *)
let test_interp_if_true =
  QCheck.Test.make
    ~count:200
    ~name:"interpreter: if nonzero then a else b = a"
    (QCheck.make
       QCheck.Gen.(pair (int_range 1 100) (int_range 0 100))
       ~print:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b))
    (fun (a, b) ->
      let expr = If (Int 1, Int a, Int b) in
      match run ~output:no_output 1000 expr with
      | Ok result -> result = a
      | Error _ -> false)
;;

(** If with zero condition chooses else-branch *)
let test_interp_if_false =
  QCheck.Test.make
    ~count:200
    ~name:"interpreter: if 0 then a else b = b"
    (QCheck.make
       QCheck.Gen.(pair (int_range 0 100) (int_range 0 100))
       ~print:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b))
    (fun (a, b) ->
      let expr = If (Int 0, Int a, Int b) in
      match run ~output:no_output 1000 expr with
      | Ok result -> result = b
      | Error _ -> false)
;;

(** Step limit always catches infinite loops *)
let test_interp_step_limit =
  QCheck.Test.make
    ~count:50
    ~name:"interpreter: step limit catches infinite loop"
    (QCheck.make QCheck.Gen.(int_range 10 500) ~print:string_of_int)
    (fun limit ->
      let loop =
        LetRec ("loop", Fun ("x", App (Var "loop", Var "x")), App (Var "loop", Int 0))
      in
      match run ~output:no_output limit loop with
      | Error `Steps_exceeded -> true
      | _ -> false)
;;

(** Division by zero always returns error *)
let test_interp_div_zero =
  QCheck.Test.make
    ~count:100
    ~name:"interpreter: division by zero is error"
    (QCheck.make QCheck.Gen.(int_range 0 100) ~print:string_of_int)
    (fun n ->
      let expr = Bin (Div, Int n, Int 0) in
      match run ~output:no_output 1000 expr with
      | Error `Division_by_zero -> true
      | _ -> false)
;;

(** Applying non-function always returns error *)
let test_interp_apply_non_fun =
  QCheck.Test.make
    ~count:100
    ~name:"interpreter: applying non-function is error"
    (QCheck.make
       QCheck.Gen.(pair (int_range 0 100) (int_range 0 100))
       ~print:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b))
    (fun (a, b) ->
      let expr = App (Int a, Int b) in
      match run ~output:no_output 1000 expr with
      | Error (`Type_error _) -> true
      | _ -> false)
;;

(** Parsed-and-interpreted arithmetic roundtrip:
    parse a printed arith expr, interpret it, compare with reference *)
let test_parse_interp_arith =
  QCheck.Test.make
    ~count:300
    ~name:"parser+interpreter: parse(print(arith)) evaluates correctly"
    arbitrary_arith
    (fun ast ->
       let printed = to_string ast in
       match parse printed with
       | Error _ -> false
       | Ok parsed ->
         (match eval_arith ast, run ~output:no_output 10000 parsed with
          | Some expected, Ok actual -> expected = actual
          | None, _ -> true
          | _ -> false))
;;

let () =
  let suite =
    [ (* Parser & Printer *)
      test_roundtrip
    ; test_print_deterministic
    ; test_print_parseable
    ; test_double_roundtrip (* Interpreter *)
    ; test_interp_arith
    ; test_interp_identity
    ; test_interp_const
    ; test_interp_let_id
    ; test_interp_if_true
    ; test_interp_if_false
    ; test_interp_step_limit
    ; test_interp_div_zero
    ; test_interp_apply_non_fun (* Parser + Interpreter *)
    ; test_parse_interp_arith
    ]
  in
  exit (QCheck_base_runner.run_tests suite)
;;
