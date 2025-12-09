[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base
open Angstrom

let whitespace = take_while Char.is_whitespace
let whitespace1 = take_while1 Char.is_whitespace
let keyword s = string s <* whitespace1

let keyword1 s =
  string s
  <* (whitespace1 *> return ()
      <|> (peek_char
           >>= function
           | Some '(' -> return ()
           | _ -> fail "expected whitespace or '('"))
;;

let parens p = char '(' *> whitespace *> p <* whitespace <* char ')' <* whitespace
let token p = p <* whitespace
let token1 p = p <* whitespace1

(**Constatnts*)
let parse_integer =
  token (take_while1 Char.is_digit) >>| fun digits -> Const (CInt (Int.of_string digits))
;;

let is_name_start = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords = [ "let"; "rec"; "in"; "if"; "then"; "else"; "fun"; "true"; "false"; "not" ]

let parse_name =
  let* first = satisfy is_name_start in
  let* rest = take_while is_name_char in
  let* _ = whitespace in
  let name = String.of_char first ^ rest in
  if List.mem keywords name ~equal:String.equal
  then fail ("keyword " ^ name ^ " cannot be an identifier")
  else return (Var name)
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

(**Unary operators *)
let neg_op = token (string "-") *> return Neg

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
let parse_pattern =
  choice
    [ (let* first = satisfy is_name_start in
       let* rest = take_while is_name_char in
       let* _ = whitespace in
       let name = String.of_char first ^ rest in
       if List.mem keywords name ~equal:String.equal || String.equal name "_"
       then fail ("keyword " ^ name ^ " cannot be an identifier")
       else return (PVar name))
    ; (token1 (char '_') >>| fun _ -> PAny)
    ; (token1 (string "()") >>| fun _ -> PUnit)
    ]
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
  let* params = many1 parse_pattern in
  let* _ = token (string "->") in
  let* body = expr in
  return (FunExpr (params, body))
;;

let parse_app atom =
  let* first = atom in
  let* rest = many atom in
  return (List.fold_left ~f:(fun acc arg -> App (acc, arg)) ~init:first rest)
;;

let parse_let expr =
  let* is_rec = kw_let *> option false (kw_rec *> return true) in
  let* pat = parse_pattern in
  let* params = many parse_pattern in
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
      choice [ (neg_op *> unary_rec >>| fun e -> UnOp (Neg, e)); token base_parser ])
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
  compare_ops
;;

let parse_expr =
  fix (fun parse_expr ->
    let atom = choice [ parse_unit; parens parse_expr; parse_integer; parse_name ] in
    let application = parse_app atom in
    let expr_with_ops = parse_operators application in
    choice
      [ parse_if parse_expr
      ; parse_lambda parse_expr
      ; parse_let parse_expr
      ; expr_with_ops
      ])
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
  let* params = many parse_pattern in
  let* _ = token (char '=') in
  let* value = expr in
  let value_with_lambdas =
    if List.is_empty params then value else FunExpr (params, value)
  in
  return ((if is_rec then Rec else NonRec), pat, value_with_lambdas)
;;

let parse_program_item expr = parse_binding expr >>| fun e -> Value e

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
