(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_var_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_keyword = function
  | "if" | "then" | "else" | "fun" | "let" | "rec" | "in" -> true
  | _ -> false
;;

let spaces = skip_while is_space
let token p = p <* spaces
let parens p = token (char '(') *> p <* token (char ')')

let number =
  let positive =
    token
      (take_while1 (function
        | '0' .. '9' -> true
        | _ -> false))
  in
  token
    (option '+' (char '-')
     >>= fun sign ->
     positive
     >>= fun num ->
     let full_num =
       match sign with
       | '-' -> "-" ^ num
       | _ -> num
     in
     match int_of_string_opt full_num with
     | Some n -> return n
     | None -> fail "integer overflow")
;;

let number_expr = number >>| fun num -> Ast.Num num

let req_not_keyword s =
  match is_keyword s with
  | true -> fail ("reserved keyword: " ^ s)
  | false -> return s
;;

let varname = token (take_while1 is_var_char) >>= req_not_keyword
let fun_name = take_while1 is_var_char <* take_while1 is_space >>= req_not_keyword
let varname_expr = varname >>| fun v -> Ast.Var v

let mult_div_op =
  token
    (choice
       [ char '*' *> return Ast.Mult
       ; char '/' *> return Ast.Div
       ; char '=' *> return Ast.Equal
       ; char '<' *> return Ast.Less
       ; char '>' *> return Ast.More
       ])
;;

let add_sub_op = token (char '+' *> return Ast.Plus <|> char '-' *> return Ast.Minus)
let multi_fun args = List.fold_right (fun arg body -> Ast.Fun (arg, body)) args

let expr =
  fix (fun expr ->
    let fun_expr =
      token (string "fun") *> many1 varname
      >>= fun args ->
      token (string "->") *> expr >>= fun body -> return @@ multi_fun args body
    in
    let app_expr =
      choice [ fun_expr; (fun_name >>| fun n -> Ast.Var n) ]
      >>= fun name ->
      many1 (choice [ number_expr; varname_expr; parens expr ])
      >>| fun args -> List.fold_left (fun f arg -> Ast.App (f, arg)) name args
    in
    let unary_expr = choice [ number_expr; app_expr; varname_expr; parens expr ] in
    let mult_expr =
      unary_expr
      >>= fun first ->
      many (mult_div_op >>= fun op -> unary_expr >>| fun right -> op, right)
      >>| List.fold_left (fun left (op, right) -> Ast.Binop (op, left, right)) first
    in
    let add_expr =
      mult_expr
      >>= fun first ->
      many (add_sub_op >>= fun op -> mult_expr >>| fun right -> op, right)
      >>| List.fold_left (fun left (op, right) -> Ast.Binop (op, left, right)) first
    in
    let if_expr =
      token (string "if") *> expr
      >>= fun cond ->
      token (string "then") *> expr
      >>= fun then_branch ->
      token (string "else") *> expr
      >>= fun else_branch -> return @@ Ast.If (cond, then_branch, else_branch)
    in
    let let_expr =
      (string "let" <* take_while1 is_space)
      *> ((string "rec" <* take_while1 is_space) *> return true <|> return false)
      >>= fun is_rec ->
      varname
      >>= fun name ->
      many varname
      >>= fun args ->
      token (char '=') *> expr
      >>= fun value ->
      let body =
        match args with
        | [] -> value
        | _ -> multi_fun args value
      in
      token (string "in") *> expr
      >>= fun res ->
      if is_rec
      then return @@ Ast.Letrec (name, body, res)
      else return @@ Ast.Let (name, body, res)
    in
    choice [ if_expr; fun_expr; let_expr; add_expr ])
;;

type error = [ `Parsing_error of string ]

let parse str =
  match Angstrom.parse_string ~consume:Consume.All (spaces *> expr <* spaces) str with
  | Result.Ok x -> Result.Ok x
  | Error msg -> Result.Error (`Parsing_error msg)
;;
