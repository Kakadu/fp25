(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

let gen_identifier =
  QCheck.Gen.(
    map2
      (fun c cs -> String.make 1 c ^ String.concat "" (List.map (String.make 1) cs))
      (oneofl [ 'a'; 'b'; 'c'; 'f'; 'g'; 'x'; 'y'; 'z' ])
      (list_size (0 -- 3) (oneofl [ 'a'; 'x'; 'y'; 'z'; '0'; '1' ])))
;;

let gen_small_int = QCheck.Gen.(int_range (-100) 100)
let gen_binop = QCheck.Gen.oneofl Ast.[ Add; Sub; Mul; Div; Eq; Lt; Gt; Le; Ge ]

let gen_expr =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      if n <= 0
      then
        frequency
          [ 3, map (fun i -> Ast.Const i) gen_small_int
          ; 2, map (fun v -> Ast.Var v) gen_identifier
          ]
      else
        frequency
          [ 1, map (fun i -> Ast.Const i) gen_small_int
          ; 1, map (fun v -> Ast.Var v) gen_identifier
          ; 2, map2 (fun v e -> Ast.Abs (v, e)) gen_identifier (self (n / 2))
          ; 2, map2 (fun e1 e2 -> Ast.App (e1, e2)) (self (n / 2)) (self (n / 2))
          ; ( 1
            , map3
                (fun op e1 e2 -> Ast.BinOp (op, e1, e2))
                gen_binop
                (self (n / 2))
                (self (n / 2)) )
          ; ( 1
            , map3
                (fun c t e -> Ast.If (c, t, e))
                (self (n / 2))
                (self (n / 2))
                (self (n / 2)) )
          ; ( 1
            , map3
                (fun v e1 e2 -> Ast.Let (v, e1, e2))
                gen_identifier
                (self (n / 2))
                (self (n / 2)) )
          ]))
;;

let test_print_parse =
  QCheck.Test.make
    ~count:1000
    ~name:"print then parse"
    (QCheck.make gen_expr)
    (fun expr ->
       let printed = Printast.show expr in
       match Parser.parse printed with
       | Ok parsed -> String.equal printed (Printast.show parsed)
       | Error _ -> true)
;;

let test_parse_valid =
  QCheck.Test.make
    ~count:1
    ~name:"parse valid"
    (QCheck.Gen.oneofl
       [ "42"; "(1 + 2)"; "x"; "(fun x -> x)"; "let x = 5 in x"; "if 1 then 2 else 3" ]
     |> QCheck.make)
    (fun s ->
      match Parser.parse s with
      | Ok _ -> true
      | Error _ -> QCheck.Test.fail_reportf "Failed: %s" s)
;;

let test_print_const =
  QCheck.Test.make ~count:100 ~name:"print const" (QCheck.make gen_small_int) (fun i ->
    String.equal (Printast.show (Ast.Const i)) (string_of_int i))
;;

let test_print_var =
  QCheck.Test.make ~count:100 ~name:"print var" (QCheck.make gen_identifier) (fun v ->
    String.equal (Printast.show (Ast.Var v)) v)
;;

let test_roundtrip =
  let cases =
    Ast.
      [ Const 42, "42"
      ; Var "x", "x"
      ; BinOp (Add, Const 1, Const 2), "(1 + 2)"
      ; App (Var "f", Const 5), "(f 5)"
      ]
  in
  QCheck.Test.make
    ~count:1
    ~name:"roundtrip"
    (QCheck.Gen.oneofl cases |> QCheck.make)
    (fun (expr, exp) ->
      String.equal (Printast.show expr) exp
      &&
      match Parser.parse exp with
      | Ok _ -> true
      | Error _ -> false)
;;

let test_pp_verbose =
  QCheck.Test.make ~count:100 ~name:"pp_verbose" (QCheck.make gen_expr) (fun expr ->
    let buf = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buf in
    Printast.pp_verbose fmt expr;
    Format.pp_print_flush fmt ();
    match Parser.parse (Buffer.contents buf) with
    | Ok _ -> true
    | Error _ -> false)
;;

let test_show_compact =
  QCheck.Test.make ~count:50 ~name:"show compact" (QCheck.make gen_small_int) (fun i ->
    let expr = Ast.Const i in
    String.equal (Printast.show expr) (Printast.show ~compact:true expr))
;;

let test_binop_print =
  QCheck.Test.make ~count:1 ~name:"binop print" (QCheck.make gen_small_int) (fun _ ->
    let ops =
      Ast.
        [ Add, "+"
        ; Sub, "-"
        ; Mul, "*"
        ; Div, "/"
        ; Eq, "="
        ; Lt, "<"
        ; Gt, ">"
        ; Le, "<="
        ; Ge, ">="
        ]
    in
    List.for_all
      (fun (op, sym) ->
        let printed = Printast.show (Ast.BinOp (op, Const 1, Const 2)) in
        String.contains printed (String.get sym 0))
      ops)
;;

let test_if_print =
  QCheck.Test.make ~count:10 ~name:"if print" (QCheck.make gen_small_int) (fun i ->
    let expr = Ast.If (Const i, Const 1, Const 0) in
    let printed = Printast.show expr in
    String.contains printed 'i' && String.contains printed 'f')
;;

let test_let_print =
  QCheck.Test.make ~count:10 ~name:"let print" (QCheck.make gen_identifier) (fun x ->
    let expr = Ast.Let (x, Const 42, Var x) in
    let printed = Printast.show expr in
    String.contains printed 'l'
    && String.contains printed 'e'
    && String.contains printed 't')
;;

let test_letrec_print =
  QCheck.Test.make ~count:20 ~name:"letrec print" (QCheck.make gen_small_int) (fun n ->
    let expr = Ast.LetRec ("f", "n", Var "n", Const n) in
    let printed = Printast.show expr in
    String.contains printed 'r'
    && String.contains printed 'e'
    && String.contains printed 'c')
;;

let test_fix_print =
  QCheck.Test.make ~count:10 ~name:"fix print" (QCheck.make gen_identifier) (fun x ->
    let expr = Ast.Fix (Abs (x, Var x)) in
    let printed = Printast.show expr in
    String.contains printed 'f'
    && String.contains printed 'i'
    && String.contains printed 'x')
;;

let test_prim_print =
  QCheck.Test.make ~count:10 ~name:"prim print" (QCheck.make gen_small_int) (fun i ->
    let expr = Ast.Prim ("println_int", [ Const i ]) in
    let printed = Printast.show expr in
    String.contains printed 'p')
;;

let test_nested_if =
  QCheck.Test.make ~count:15 ~name:"nested if" (QCheck.make gen_small_int) (fun n ->
    let expr = Ast.If (Const n, If (Const 1, Const 2, Const 3), Const 4) in
    match Parser.parse (Printast.show expr) with
    | Ok _ -> true
    | Error _ -> false)
;;

let test_nested_let =
  QCheck.Test.make ~count:15 ~name:"nested let" (QCheck.make gen_identifier) (fun x ->
    let expr = Ast.Let (x, Const 1, Let ("y", Var x, Let ("z", Var "y", Var "z"))) in
    String.contains (Printast.show expr) 'l')
;;

let test_church_encodings =
  QCheck.Test.make ~count:1 ~name:"church encodings" QCheck.unit (fun () ->
    let open Ast in
    let terms =
      [ Abs ("x", Abs ("y", Var "x")) (* true *)
      ; Abs ("x", Abs ("y", Var "y")) (* false *)
      ; Abs ("f", Abs ("x", Var "x")) (* zero *)
      ; Abs ("f", Abs ("x", App (Var "f", Var "x"))) (* one *)
      ; Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))) (* two *)
      ]
    in
    List.for_all (fun t -> String.length (Printast.show ~compact:true t) > 0) terms)
;;

let test_comparison_ops =
  QCheck.Test.make
    ~count:30
    ~name:"comparison ops"
    QCheck.(pair (make gen_small_int) (make gen_small_int))
    (fun (a, b) ->
      let ops = Ast.[ Eq, "="; Lt, "<"; Gt, ">"; Le, "<="; Ge, ">=" ] in
      List.for_all
        (fun (op, sym) ->
          let printed = Printast.show (BinOp (op, Const a, Const b)) in
          String.contains printed (String.get sym 0))
        ops)
;;

let test_all_binops =
  QCheck.Test.make
    ~count:50
    ~name:"all binops"
    QCheck.(pair (make gen_expr) (make gen_expr))
    (fun (e1, e2) ->
      let ops = Ast.[ Add; Sub; Mul; Div; Eq; Lt; Gt; Le; Ge ] in
      List.for_all (fun op -> String.length (Printast.show (BinOp (op, e1, e2))) > 0) ops)
;;

let test_multi_abs =
  QCheck.Test.make ~count:15 ~name:"multi abs" QCheck.unit (fun () ->
    let open Ast in
    let terms =
      [ Abs ("a", Abs ("b", Abs ("c", Var "a")))
      ; Abs ("a", Abs ("b", Abs ("c", Abs ("d", Var "a"))))
      ]
    in
    List.for_all (fun t -> String.length (Printast.show ~compact:true t) > 0) terms)
;;

let test_prim_multiple =
  QCheck.Test.make ~count:20 ~name:"prim multi" (QCheck.make gen_small_int) (fun n ->
    let prims =
      Ast.
        [ "println_int", [ Const n ]
        ; "print", [ Const n; Var "x" ]
        ; "debug", [ Var "a"; Var "b"; Const 0 ]
        ]
    in
    List.for_all
      (fun (name, args) -> String.length (Printast.show (Prim (name, args))) > 0)
      prims)
;;

let () =
  let suite =
    [ test_print_const
    ; test_print_var
    ; test_roundtrip
    ; test_parse_valid
    ; test_print_parse
    ; test_pp_verbose
    ; test_show_compact
    ; test_binop_print
    ; test_if_print
    ; test_let_print
    ; test_letrec_print
    ; test_fix_print
    ; test_prim_print
    ; test_nested_if
    ; test_nested_let
    ; test_church_encodings
    ; test_comparison_ops
    ; test_all_binops
    ; test_multi_abs
    ; test_prim_multiple
    ]
  in
  QCheck_base_runner.run_tests_main suite
;;
