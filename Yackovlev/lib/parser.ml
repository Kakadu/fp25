(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let token p = p <* spaces
let symbol s = token (string s)
let parens p = symbol "(" *> p <* symbol ")"

let is_ident_start = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let ident_raw =
  satisfy is_ident_start
  >>= fun first -> take_while is_ident_char >>| fun rest -> String.make 1 first ^ rest
;;

let keywords = [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else" ]

let keyword s =
  token ident_raw
  >>= fun id -> if id = s then return () else fail ("Expected keyword " ^ s)
;;

let ident =
  token ident_raw
  >>= fun s ->
  if List.mem s keywords then fail "Keyword cannot be used as identifier" else return s
;;

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
  |> token
;;

type error = Parsing_error of string

let pp_error ppf (Parsing_error s) = Format.fprintf ppf "%s" s

let expr : Ast.expr Angstrom.t =
  let open Ast in
  fix (fun expr ->
    let chainl1 p op =
      p
      >>= fun init ->
      many (op >>= fun f -> p >>| fun v -> f, v)
      >>| fun rest -> List.fold_left (fun acc (f, v) -> f acc v) init rest
    in
    let atom =
      choice [ parens expr; (integer >>| fun n -> Int n); (ident >>| fun x -> Var x) ]
    in
    let app =
      many1 atom
      >>= function
      | [ x ] -> return x
      | x :: xs -> return (List.fold_left (fun acc r -> App (acc, r)) x xs)
      | [] -> fail "application on empty list, impossible by [many1]"
    in
    let unary =
      fix (fun self ->
        choice
          [ (symbol "-" *> self
             >>| function
             | Int n -> Int (-n)
             | e -> Binop (Sub, Int 0, e))
          ; app
          ])
    in
    let mul_div =
      let op =
        choice
          [ symbol "*" *> return (fun l r -> Binop (Mul, l, r))
          ; symbol "/" *> return (fun l r -> Binop (Div, l, r))
          ]
      in
      chainl1 unary op
    in
    let add_sub =
      let op =
        choice
          [ symbol "+" *> return (fun l r -> Binop (Add, l, r))
          ; symbol "-" *> return (fun l r -> Binop (Sub, l, r))
          ]
      in
      chainl1 mul_div op
    in
    let cmp =
      let op =
        choice
          [ symbol "=" *> return (fun l r -> Cmp (Eq, l, r))
          ; symbol "<>" *> return (fun l r -> Cmp (Neq, l, r))
          ; symbol "<=" *> return (fun l r -> Cmp (Le, l, r))
          ; symbol ">=" *> return (fun l r -> Cmp (Ge, l, r))
          ; symbol "<" *> return (fun l r -> Cmp (Lt, l, r))
          ; symbol ">" *> return (fun l r -> Cmp (Gt, l, r))
          ]
      in
      chainl1 add_sub op
    in
    let make_fun params body = List.fold_right (fun x acc -> Abs (x, acc)) params body in
    let fun_expr =
      keyword "fun" *> many1 ident
      >>= fun params -> symbol "->" *> expr >>| fun body -> make_fun params body
    in
    let if_expr =
      keyword "if" *> expr
      >>= fun cond ->
      keyword "then" *> expr
      >>= fun then_ -> keyword "else" *> expr >>| fun else_ -> If (cond, then_, else_)
    in
    let let_expr =
      keyword "let" *> token ident_raw
      >>= fun next_token ->
      let is_rec = next_token = "rec" in
      (if is_rec
       then ident
       else if List.mem next_token keywords
       then fail "Keyword used as variable name"
       else return next_token)
      >>= fun name ->
      many ident
      >>= fun params ->
      symbol "=" *> expr
      >>= fun rhs ->
      keyword "in" *> expr
      >>| fun body ->
      let rhs' = make_fun params rhs in
      if is_rec then Let_rec (name, rhs', body) else Let (name, rhs', body)
    in
    choice [ let_expr; if_expr; fun_expr; cmp ])
;;

let parse str =
  match
    Angstrom.parse_string (spaces *> expr <* spaces) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (Parsing_error er)
;;
