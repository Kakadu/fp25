[@@@ocaml.text "/*"]

(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

let ( let* ) = ( >>= )

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let ws = skip_while is_whitespace
let parens p = char '(' *> p <* char ')' <* ws

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let var = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let var_and_digit c = var c || is_digit c
let keys = [ "then"; "if"; "else"; "let"; "rec"; "in"; "fun"; "true"; "false" ]

let chainl1 e op =
  let rec l acc =
    (let* f = op in
     let* x = e in
     l (f acc x))
    <|> return acc
  in
  e >>= l
;;

let sym s = string s <* ws

let kw s =
  let* _ = string s in
  let* next = peek_char in
  match next with
  | Some c when var_and_digit c -> fail "keyword"
  | _ -> ws
;;

let parse_minus_op =
  let* _ = sym "-" in
  ws *> return (fun left right -> BinOp (Sub, left, right))
;;

let parse_add_op =
  let* _ = sym "+" in
  ws *> return (fun left right -> BinOp (Add, left, right))
;;

let parse_mul_op =
  let* _ = sym "*" in
  ws *> return (fun left right -> BinOp (Mul, left, right))
;;

let parse_div_op =
  let* _ = sym "/" in
  ws *> return (fun left right -> BinOp (Div, left, right))
;;

let parse_lt_op =
  let* _ = sym "<" in
  ws *> return (fun left right -> BinOp (Lt, left, right))
;;

let parse_eq_op =
  let* _ = sym "=" in
  ws *> return (fun left right -> BinOp (Eq, left, right))
;;

let parse_mt_op =
  let* _ = sym ">" in
  ws *> return (fun left right -> BinOp (Mt, left, right))
;;

let is_keyword_next_helper input key =
  let k_len = String.length key in
  String.starts_with ~prefix:key input
  && (String.length input = k_len || not (var_and_digit input.[k_len]))
;;

let is_keyword_next =
  let* n = available in
  let* s = peek_string (min n 10) in
  return (List.exists (is_keyword_next_helper s) keys)
;;

let parse_int =
  let* n = take_while1 is_digit >>| int_of_string <* ws in
  return (Const n)
;;

let parse_ident =
  let* head =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* tail = take_while var_and_digit in
  let s = String.make 1 head ^ tail in
  if List.mem s keys then fail "keyword" else return s <* ws
;;

let parse_var =
  let* v = parse_ident <* ws in
  return (Var v)
;;

let parse_fun_helper params body =
  List.fold_right (fun param acc -> Fun (param, acc)) params body
;;

let rec parse_atom () =
  choice [ parse_int; parse_var; parens (return () >>= fun () -> parse_expr ()) ]

and parse_factor () =
  choice
    [ (let* _ = sym "-" in
       parse_factor () >>| fun e -> Neg e)
    ; parse_atom ()
    ]

and parse_terms () = chainl1 (parse_app ()) (parse_mul_op <|> parse_div_op)

and parse_compare () =
  chainl1 (parse_arithmetic ()) (parse_eq_op <|> parse_lt_op <|> parse_mt_op)

and parse_app () =
  let* head = parse_factor () in
  let rec loop acc =
    let* is_kwd = is_keyword_next in
    if is_kwd
    then return acc
    else
      (let* arg = parse_atom () in
       loop (App (acc, arg)))
      <|> return acc
  in
  loop head

and parse_arithmetic () = chainl1 (parse_terms ()) (parse_add_op <|> parse_minus_op)

and parse_let () =
  let* _ = kw "let" in
  let* rec_flag = kw "rec" *> return Rec <|> return Val in
  let* name = parse_ident <* ws in
  let* args = many (parse_ident <* ws) in
  let* _ = sym "=" in
  let* let_expr = parse_expr () in
  let* _ = kw "in" in
  let* body = parse_expr () in
  return (Let (rec_flag, name, parse_fun_helper args let_expr, body))

and parse_if () =
  let* _ = kw "if" in
  let* cond = parse_expr () in
  let* _ = kw "then" in
  let* then_body = parse_expr () in
  let* _ = kw "else" in
  let* else_body = parse_expr () in
  return (If (cond, then_body, else_body))

and parse_fun () =
  let* _ = kw "fun" in
  let* args = many1 (parse_ident <* ws) in
  let* _ = sym "->" in
  let* body = parse_expr () in
  return (parse_fun_helper args body)

and parse_expr () = choice [ parse_let (); parse_if (); parse_fun (); parse_compare () ]

let parse str =
  match parse_string ~consume:Consume.All (parse_expr ()) str with
  | Ok x -> Ok x
  | Error er -> Error (`Parsing_error er)
;;
