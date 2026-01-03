[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Ast
open QCheck

let keywords =
  [ "let"
  ; "rec"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "match"
  ; "with"
  ; "fun"
  ; "function"
  ; "type"
  ; "class"
  ; "method"
  ; "new"
  ; "end"
  ; "object"
  ; "inherit"
  ; "val"
  ; "true"
  ; "false"
  ; "not"
  ]
;;

let gen_name =
  let open Gen in
  string_size ~gen:(char_range 'a' 'z') (int_range 1 3)
  |> map (fun s -> if List.mem s keywords then s ^ "x" else s)
;;

let gen_class_name =
  let open Gen in
  let* first = char_range 'A' 'Z' in
  let* rest = string_size ~gen:(char_range 'a' 'z') (int_range 0 5) in
  return (String.make 1 first ^ rest)
;;

let rec gen_pattern size =
  let open Gen in
  if size <= 0
  then oneof [ map (fun s -> PVar s) gen_name; return PAny; return PUnit ]
  else (
    let sub_pat = gen_pattern (size / 2) in
    frequency
      [ 5, map (fun s -> PVar s) gen_name
      ; 2, return PAny
      ; 2, return PUnit
      ; 1, map (fun ps -> PTuple ps) (list_size (int_range 2 3) sub_pat)
      ])
;;

let rec gen_expr size =
  let open Gen in
  let gen_var = map (fun s -> Var s) gen_name in
  let gen_const_int = map (fun i -> Const (CInt i)) small_int in
  let gen_const_bool = map (fun b -> Const (CBool b)) bool in
  let gen_const_unit = return (Const CUnit) in
  if size <= 0
  then oneof [ gen_var; gen_const_int; gen_const_bool; gen_const_unit ]
  else (
    let sub_gen = gen_expr (size / 2) in
    let sub_pat = gen_pattern (size / 2) in
    let gen_unop = map2 (fun op e -> UnOp (op, e)) (oneofl [ Neg; Not ]) sub_gen in
    let gen_binop =
      map3
        (fun op e1 e2 -> BinOp (op, e1, e2))
        (oneofl [ Add; Sub; Mul; Div; Eq; Neq; Lt; Le; Gt; Ge; And; Or ])
        sub_gen
        sub_gen
    in
    let gen_if = map3 (fun c t f -> If (c, t, f)) sub_gen sub_gen sub_gen in
    let gen_app =
      map2
        (fun f arg ->
          match f with
          | FieldAccess (obj, name) -> MethodCall (obj, name, [ arg ])
          | MethodCall (obj, name, args) -> MethodCall (obj, name, args @ [ arg ])
          | _ -> App (f, arg))
        sub_gen
        sub_gen
    in
    let gen_let =
      oneof
        [ map3 (fun p e1 e2 -> Let (NonRec, p, e1, e2)) sub_pat sub_gen sub_gen
        ; map3 (fun name e1 e2 -> Let (Rec, PVar name, e1, e2)) gen_name sub_gen sub_gen
        ]
    in
    let gen_fun =
      map2 (fun ps e -> FunExpr (ps, e)) (list_size (int_range 1 3) sub_pat) sub_gen
    in
    let gen_tuple = map (fun es -> Tuple es) (list_size (int_range 2 4) sub_gen) in
    let gen_new =
      map2
        (fun name args -> New (name, args))
        gen_class_name
        (list_size (int_range 0 2) sub_gen)
    in
    let gen_field_access =
      map2 (fun obj field -> FieldAccess (obj, field)) sub_gen gen_name
    in
    let gen_method_call =
      map3
        (fun obj method_name args -> MethodCall (obj, method_name, args))
        sub_gen
        gen_name
        (list_size (int_range 1 2) sub_gen)
    in
    frequency
      [ 5, gen_const_int
      ; 3, gen_const_bool
      ; 1, gen_const_unit
      ; 5, gen_var
      ; 3, gen_unop
      ; 3, gen_binop
      ; 2, gen_if
      ; 2, gen_app
      ; 2, gen_let
      ; 2, gen_fun
      ; 1, gen_tuple
      ; 1, gen_new
      ; 1, gen_field_access
      ; 1, gen_method_call
      ])
;;

let rec shrink_expr = function
  | Const _ | Var _ -> Iter.empty
  | UnOp (op, e) ->
    let open Iter in
    return e <+> (shrink_expr e >|= fun e' -> UnOp (op, e'))
  | BinOp (binop, e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> BinOp (binop, e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> BinOp (binop, e1, e2'))
  | If (c, t, f) ->
    let open Iter in
    of_list [ t; f ]
    <+> (shrink_expr c >|= fun c' -> If (c', t, f))
    <+> (shrink_expr t >|= fun t' -> If (c, t', f))
    <+> (shrink_expr f >|= fun f' -> If (c, t, f'))
  | App (e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> App (e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> App (e1, e2'))
  | Let (_, _, e1, e2) ->
    let open Iter in
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun e1' -> Let (NonRec, PVar "x", e1', e2))
    <+> (shrink_expr e2 >|= fun e2' -> Let (NonRec, PVar "x", e1, e2'))
  | FunExpr (_, e) ->
    let open Iter in
    return e <+> (shrink_expr e >|= fun e' -> FunExpr ([ PVar "x" ], e'))
  | Tuple es ->
    let open Iter in
    (match es with
     | [] -> empty
     | [ single ] -> return single
     | _ -> of_list es)
  | New (_, fields) ->
    let open Iter in
    if List.length fields > 0 then return (New ("C", [])) else empty
  | MethodCall (obj, _, _) ->
    let open Iter in
    return obj <+> (shrink_expr obj >|= fun obj' -> FieldAccess (obj', "f"))
  | FieldAccess (obj, _) ->
    let open Iter in
    return obj <+> (shrink_expr obj >|= fun obj' -> FieldAccess (obj', "f"))
;;

let gen_binding size =
  let open Gen in
  let* rf = oneofl [ NonRec; Rec ] in
  let* p =
    match rf with
    | Rec ->
      gen_pattern (size / 2)
      >>= fun p ->
      (match p with
       | PVar _ -> return p
       | _ -> map (fun name -> PVar name) gen_name)
    | NonRec -> gen_pattern (size / 2)
  in
  let* e = gen_expr (size / 2) in
  return (rf, p, e)
;;

let gen_method_def size =
  let open Gen in
  let* method_name = gen_name in
  let* method_params = list_size (int_range 0 2) (gen_pattern 0) in
  let* method_body = gen_expr (size / 2) in
  return { method_name; method_params; method_body }
;;

let gen_class_def size =
  let open Gen in
  let* class_name = gen_class_name in
  let* parent_class = option gen_class_name in
  let* self_name = option gen_name in
  let* fields = list_size (int_range 0 2) (pair gen_name (gen_expr (size / 2))) in
  let* methods = list_size (int_range 0 2) (gen_method_def size) in
  return { class_name; parent_class; self_name; fields; methods }
;;

let gen_structure_item size =
  let open Gen in
  frequency
    [ 3, map (fun b -> Value b) (gen_binding size)
    ; 1, map (fun c -> ClassDef c) (gen_class_def size)
    ]
;;

let gen_program =
  let open Gen in
  sized (fun size -> list_size (int_range 1 5) (gen_structure_item size))
;;

let shrink_binding (rf, p, e) =
  let open Iter in
  shrink_expr e >|= fun e' -> rf, p, e'
;;

let shrink_structure_item = function
  | Value b -> Iter.map (fun b' -> Value b') (shrink_binding b)
  | ClassDef _ -> Iter.empty
;;

let shrink_program items = Shrink.list ~shrink:shrink_structure_item items
let arb_program = make ~print:show_program ~shrink:shrink_program gen_program
let arb_expr = make ~print:show_expr ~shrink:shrink_expr (Gen.sized gen_expr)

let parser_test =
  Test.make ~count:1000 ~name:"parser round-trip property" arb_expr (fun expr ->
    let code_string = pretty_print_expr expr in
    match parse code_string with
    | Ok parsed_expr ->
      if Stdlib.( = ) expr parsed_expr
      then true
      else
        Test.fail_reportf
          "AST mismatch after round-trip.\n\
           Original AST:   %s\n\
           Generated Code: %s\n\
           Parsed AST:     %s"
          (show_expr expr)
          code_string
          (show_expr parsed_expr)
    | Error msg ->
      Test.fail_reportf
        "Parse failed on generated code.\n\
         Original AST:   %s\n\
         Code:           %s\n\
         Error:          %s"
        (show_expr expr)
        code_string
        msg)
;;

let program_test =
  Test.make
    ~count:1000
    ~name:"parser round-trip property (program)"
    arb_program
    (fun prog ->
       let code_string = pretty_print_program prog in
       match parse_structure_items code_string with
       | Ok parsed_prog ->
         if Stdlib.( = ) prog parsed_prog
         then true
         else
           Test.fail_reportf
             "Program AST mismatch after round-trip.\n\
              Original AST:   %s\n\
              Generated Code: %s\n\
              Parsed AST:     %s"
             (show_program prog)
             code_string
             (show_program parsed_prog)
       | Error msg ->
         Test.fail_reportf
           "Parse failed on generated program code.\n\
            Original AST:   %s\n\
            Code:           %s\n\
            Error:          %s"
           (show_program prog)
           code_string
           msg)
;;

let () = QCheck_runner.run_tests_main [ parser_test; program_test ]
