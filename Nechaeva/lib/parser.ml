(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let token p = spaces *> p <* spaces
let parens p = char '(' *> token p <* char ')'

let is_keyword = function
  | "let" | "in" | "rec" | "if" | "then" | "else" | "fun" -> true
  | _ -> false
;;

let is_alpha =
  satisfy (function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false)
;;

let is_digit =
  satisfy (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let ident_start = is_alpha <|> char '_'
let ident_cont = is_alpha <|> is_digit <|> char '_'

let ident =
  token
    (let* h = ident_start in
     let* tl = many ident_cont in
     let str = String.of_seq (List.to_seq (h :: tl)) in
     if is_keyword str
     then fail ("keyword '" ^ str ^ "' cannot be used as identifier")
     else return str)
;;

let var_parser = ident >>| fun name -> Var name

let number =
  token
    (take_while1 (function
       | '0' .. '9' -> true
       | _ -> false)
     >>= fun digits ->
     peek_char
     >>= (function
            | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
              fail "Invalid number format"
            | _ -> return digits)
     >>| int_of_string)
  >>| fun num -> Const (Int num)
;;

let plus = token (char '+') *> return Plus
let minus = token (char '-') *> return Minus
let mul = token (char '*') *> return Mul
let div = token (char '/') *> return Div
let eq = token (char '=') *> return Equal
let neq = token (string "<>") *> return NotEqual
let lt = token (char '<') *> return Less
let le = token (string "<=") *> return LessEq
let gt = token (char '>') *> return Greater
let ge = token (string ">=") *> return GreaterEq

let expr =
  fix (fun expr ->
    let atom = choice [ number; var_parser; parens expr ] in
    let unary =
      let neg = token (char '-') *> atom >>| fun e -> UnOp (Neg, e) in
      choice [ neg; atom ]
    in
    let app =
      unary
      >>= fun func ->
      many (token atom) >>| List.fold_left (fun f arg -> App (f, arg)) func
    in
    let product =
      app
      >>= fun first ->
      many (choice [ mul; div ] >>= fun op -> app >>| fun right -> op, right)
      >>| List.fold_left (fun left (op, right) -> BinOp (op, left, right)) first
    in
    let sum =
      product
      >>= fun first ->
      many (choice [ plus; minus ] >>= fun op -> product >>| fun right -> op, right)
      >>| List.fold_left (fun left (op, right) -> BinOp (op, left, right)) first
    in
    let comparison =
      sum
      >>= fun left ->
      option
        left
        (choice [ neq; le; ge; lt; gt; eq ]
         >>= fun op -> sum >>| fun right -> Comp (op, left, right))
    in
    let let_expr =
      spaces *> string "let"
      >>= fun _ ->
      take_while1 is_space
      >>= fun _ ->
      token (string "rec")
      >>| (fun _ -> Recursive)
      <|> return NonRecursive
      >>= fun rec_flag ->
      ident
      >>= fun name ->
      many ident
      >>= fun args ->
      token (char '=')
      >>= fun _ ->
      expr
      >>= fun value ->
      token (string "in")
      >>= fun _ ->
      expr
      >>| fun in_expr ->
      let body =
        match args with
        | [] -> value
        | _ -> Abs (args, value)
      in
      Let (rec_flag, name, body, in_expr)
    in
    let if_expr =
      token (string "if")
      >>= fun _ ->
      expr
      >>= fun cond ->
      token (string "then")
      >>= fun _ ->
      expr
      >>= fun then_branch ->
      token (string "else")
      >>= fun _ -> expr >>| fun else_branch -> If (cond, then_branch, else_branch)
    in
    let abs_expr =
      token (string "fun")
      >>= fun _ ->
      many1 ident
      >>= fun args ->
      token (string "->") >>= fun _ -> expr >>| fun body -> Abs (args, body)
    in
    choice [ let_expr; if_expr; abs_expr; comparison ])
;;

type error = [ `Parsing_error of string ]

let parse str =
  match Angstrom.parse_string expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "syntax error")
;;
