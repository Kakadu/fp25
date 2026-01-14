[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base
open Angstrom

let whitespace = take_while Char.is_whitespace

let is_name_start = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keyword s =
  string s
  <* (peek_char
      >>= function
      | Some c when is_name_char c ->
        fail ("keyword " ^ s ^ " cannot be a prefix of identifier")
      | _ -> return ())
  <* whitespace
;;

let keyword1 = keyword
let parens p = char '(' *> whitespace *> p <* whitespace <* char ')' <* whitespace
let token p = p <* whitespace

let parse_integer =
  token (take_while1 Char.is_digit) >>| fun digits -> Const (CInt (Int.of_string digits))
;;

let parse_boolean =
  choice
    [ (keyword1 "true" >>| fun _ -> Const (CBool true))
    ; (keyword1 "false" >>| fun _ -> Const (CBool false))
    ]
;;

let keywords =
  [ "let"
  ; "rec"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "fun"
  ; "true"
  ; "false"
  ; "not"
  ; "class"
  ; "new"
  ; "method"
  ; "val"
  ; "object"
  ; "end"
  ; "inherit"
  ]
;;

let parse_ident_raw =
  let* first = satisfy is_name_start in
  let* rest = take_while is_name_char in
  let name = String.of_char first ^ rest in
  if List.mem keywords name ~equal:String.equal
  then fail ("keyword " ^ name ^ " cannot be an identifier")
  else return name
;;

let parse_ident = parse_ident_raw <* whitespace
let parse_name = parse_ident >>| fun name -> Var name

let parse_class_name =
  let* first =
    satisfy (function
      | 'A' .. 'Z' -> true
      | _ -> false)
  in
  let* rest = take_while is_name_char in
  let* _ = whitespace in
  return (String.of_char first ^ rest)
;;

let parse_field_def expr =
  let* _ = keyword "val" in
  let* first = satisfy is_name_start in
  let* rest = take_while is_name_char in
  let* _ = whitespace in
  let name = String.of_char first ^ rest in
  let* _ = token (char '=') in
  let* init_expr = expr in
  let* _ = whitespace in
  return (name, init_expr)
;;

(**Binary operators *)
let add_op = token (string "+") *> return Add

let sub_op = token (string "-") *> return Sub
let mul_op = token (string "*") *> return Mul
let div_op = token (string "/") *> return Div
let eq_op = token (string "=") *> return Eq
let neq_op = token (string "<>") *> return Neq
let lt_op = token (string "<") *> return Lt
let le_op = token (string "<=") *> return Le
let gt_op = token (string ">") *> return Gt
let ge_op = token (string ">=") *> return Ge
let and_op = token (string "&&") *> return And
let or_op = token (string "||") *> return Or

(**Unary operators *)
let neg_op = token (string "-") *> return Neg

let not_op = keyword1 "not" *> return Not

(**Keywords*)

let kw_let = keyword "let"
let kw_rec = keyword "rec"
let kw_in = keyword "in"
let kw_if = keyword1 "if"
let kw_then = keyword1 "then"
let kw_else = keyword1 "else"
let kw_fun = keyword "fun"
let parse_unit = token (string "()") *> return (Const CUnit)

(** Helper to parse binary operations *)
let parse_binary_op parsed_bin_op e1 e2 = BinOp (parsed_bin_op, e1, e2)

(** Patterns *)
let mk_simple_pattern parse_pattern_rec =
  choice
    [ (token (string "()") >>| fun _ -> PUnit)
    ; (char '_'
       *> (peek_char
           >>= function
           | Some c when is_name_char c -> fail "underscore must be separated"
           | _ -> return ())
       *> whitespace
       >>| fun _ -> PAny)
    ; (let* first = satisfy is_name_start in
       let* rest = take_while is_name_char in
       let* _ = whitespace in
       let name = String.of_char first ^ rest in
       if List.mem keywords name ~equal:String.equal || String.equal name "_"
       then fail ("keyword " ^ name ^ " cannot be an identifier")
       else return (PVar name))
    ; parens parse_pattern_rec
    ]
;;

let parse_pattern =
  fix (fun parse_pattern ->
    let simple = mk_simple_pattern parse_pattern in
    sep_by1 (char ',' *> whitespace) simple
    >>= fun lst ->
    match lst with
    | [ single ] -> return single
    | _ -> return (PTuple lst))
;;

let parse_simple_pattern = mk_simple_pattern parse_pattern

let parse_method_def expr =
  let* _ = keyword "method" in
  let* first = satisfy is_name_start in
  let* rest = take_while is_name_char in
  let* _ = whitespace in
  let method_name = String.of_char first ^ rest in
  let* params = many parse_simple_pattern in
  let* _ = token (char '=') in
  let* body = expr in
  let* _ = whitespace in
  return { method_name; method_params = params; method_body = body }
;;

let parse_class_def expr =
  let* _ = keyword "class" in
  let* class_name = parse_class_name in
  let* class_params = many parse_simple_pattern in
  let* _ = token (char '=') in
  let* _ = keyword "object" in
  let* self_name = option None (parens parse_ident >>| Option.some) in
  let parse_atom =
    choice
      [ token (string "()") *> return (Const CUnit)
      ; parse_integer
      ; parse_boolean
      ; parse_name
      ; parens expr
      ]
  in
  let parse_inherit =
    let* _ = keyword "inherit" in
    let* name = parse_class_name in
    let* args = many parse_atom in
    return (Some (name, args))
  in
  let* parent = option None parse_inherit in
  let* fields_and_methods =
    many
      (choice
         [ (parse_field_def expr >>| fun f -> `Field f)
         ; (parse_method_def expr >>| fun m -> `Method m)
         ])
  in
  let* _ = keyword "end" in
  let fields =
    List.filter_map fields_and_methods ~f:(function
      | `Field f -> Some f
      | _ -> None)
  in
  let methods =
    List.filter_map fields_and_methods ~f:(function
      | `Method m -> Some m
      | _ -> None)
  in
  return { class_name; class_params; parent_class = parent; self_name; fields; methods }
;;

let parse_new expr =
  let* _ = keyword "new" in
  let* class_name = parse_class_name in
  let parse_atom =
    choice
      [ token (string "()") *> return (Const CUnit)
      ; parse_integer
      ; parse_boolean
      ; parse_name
      ; parens expr
      ]
  in
  let* args = many parse_atom in
  return (New (class_name, args))
;;

let parse_postfix base =
  many
    (char '#'
     *> let* first = satisfy is_name_start in
        let* rest = take_while is_name_char in
        let name = String.of_char first ^ rest in
        return (fun obj -> MethodCall (obj, name, [])))
  >>| fun ops -> List.fold_left ~f:(fun acc op -> op acc) ~init:base ops
;;

let parse_app_with_methods atom =
  let* first = atom in
  let* rest = many atom in
  return
    (List.fold_left
       ~f:(fun acc arg ->
         match acc with
         | MethodCall (obj, method_name, args) ->
           MethodCall (obj, method_name, args @ [ arg ])
         | _ -> App (acc, arg))
       ~init:first
       rest)
;;

let parse_if expr =
  let* _ = kw_if in
  let* cond = expr in
  let* _ = kw_then in
  let* then_branch = expr in
  let* _ = kw_else in
  let* else_branch = expr in
  return (If (cond, then_branch, else_branch))
;;

let parse_lambda expr =
  let* _ = kw_fun in
  let* params = many1 parse_simple_pattern in
  let* _ = token (string "->") in
  let* body = expr in
  return (FunExpr (params, body))
;;

let parse_let expr =
  let* is_rec = kw_let *> option false (kw_rec *> return true) in
  let* pat = parse_pattern in
  let* params = many parse_simple_pattern in
  let* _ = token (char '=') in
  let* value = expr in
  let* _ = kw_in in
  let* body = expr in
  let value_with_lambdas =
    if List.is_empty params then value else FunExpr (params, value)
  in
  return
    (if is_rec
     then Let (Rec, pat, value_with_lambdas, body)
     else Let (NonRec, pat, value_with_lambdas, body))
;;

let parse_operators base_parser =
  (* Unary operators *)
  let unary =
    fix (fun unary_rec ->
      choice
        [ (neg_op *> unary_rec >>| fun e -> UnOp (Neg, e))
        ; (not_op *> unary_rec >>| fun e -> UnOp (Not, e))
        ; token base_parser
        ])
  in
  (* helper for left accociativity *)
  let helper p op =
    let rec loop acc = lift2 (fun f x -> f acc x) op p >>= loop <|> return acc in
    p >>= loop
  in
  (* Priority of binary operators *)
  let mul_div = helper unary (choice [ mul_op; div_op ] >>| parse_binary_op) in
  let add_sub = helper mul_div (choice [ add_op; sub_op ] >>| parse_binary_op) in
  let compare_ops =
    helper
      add_sub
      (choice [ eq_op; neq_op; le_op; lt_op; ge_op; gt_op ] >>| parse_binary_op)
  in
  let logical_and = helper compare_ops (and_op >>| parse_binary_op) in
  let logical_or = helper logical_and (or_op >>| parse_binary_op) in
  logical_or
;;

let parse_name_raw = parse_ident_raw >>| fun name -> Var name

let parse_expr =
  fix (fun parse_expr ->
    let atom =
      choice
        [ parse_unit
        ; parse_boolean
        ; parse_integer
        ; parse_new parse_expr
        ; parse_name_raw
        ; parens parse_expr
        ]
    in
    let with_postfix = atom >>= parse_postfix <* whitespace in
    let application = parse_app_with_methods with_postfix in
    let expr_with_ops = parse_operators application in
    let expr_non_tuple =
      choice
        [ parse_if parse_expr
        ; parse_lambda parse_expr
        ; parse_let parse_expr
        ; expr_with_ops
        ]
    in
    sep_by1 (char ',' *> whitespace) expr_non_tuple
    >>= fun exprs ->
    match exprs with
    | [ single ] -> return single
    | _ -> return (Tuple exprs))
;;

let parse_full_expr = whitespace *> parse_expr <* whitespace <* end_of_input

let parse s =
  match Angstrom.parse_string ~consume:All parse_full_expr s with
  | Ok result -> Ok result
  | Error msg -> Error msg
;;

let parse_binding expr =
  let* is_rec = kw_let *> option false (kw_rec *> return true) in
  let* pat = parse_pattern in
  let* params = many parse_simple_pattern in
  let* _ = token (char '=') in
  let* value = expr in
  let value_with_lambdas =
    if List.is_empty params then value else FunExpr (params, value)
  in
  return ((if is_rec then Rec else NonRec), pat, value_with_lambdas)
;;

let parse_program_item expr =
  choice
    [ (parse_class_def expr >>| fun cdef -> ClassDef cdef)
    ; (parse_binding expr >>| fun e -> Value e)
    ]
;;

let parse_program =
  let items = sep_by (token (string ";;")) (parse_program_item parse_expr) in
  whitespace *> items
  <* option () (token (string ";;") *> return ())
  <* whitespace
  <* end_of_input
;;

let parse_structure_items s =
  match Angstrom.parse_string ~consume:All parse_program s with
  | Ok result -> Ok result
  | Error msg -> Error msg
;;
