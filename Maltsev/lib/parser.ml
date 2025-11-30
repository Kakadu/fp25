(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Ast
open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

let parens p = char '(' *> p <* char ')'

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha c =
  match c with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let var_name c =
  match c with
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let sign =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | _ -> return ""
;;

let parse_number =
  spaces *> sign
  >>= fun sign ->
  take_while1 is_digit
  >>| fun x ->
  match sign with
  | "-" -> Ast.Const (-1 * int_of_string x)
  | _ -> Ast.Const (int_of_string x)
;;

let is_keyword s =
  match s with
  | "let" | "rec" | "in" | "if" | "then" | "else" | "fun" | "->" -> true
  | _ -> false
;;

let parse_varname =
  spaces *> take_while1 var_name
  >>= fun s ->
  match s with
  | _ when is_keyword s -> fail "Can't have a name same as keyword"
  | _ when String.for_all is_digit s -> fail "Number cant be a name"
  | _ -> return (Ast.Ident s)
;;

let plus = spaces *> char '+' >>= fun _ -> return Plus
let minus = spaces *> char '-' >>= fun _ -> return Minus
let mul = spaces *> char '*' >>= fun _ -> return Mul
let div = spaces *> char '/' >>= fun _ -> return Div
let eq = spaces *> char '=' >>= fun _ -> return Eq
let neq = spaces *> string "!=" >>= fun _ -> return Neq
let le = spaces *> char '<' >>= fun _ -> return Le
let bi = spaces *> char '>' >>= fun _ -> return Bi

let parse_expr =
  fix (fun parse_expr ->
    let abstr =
      spaces *> string "fun"
      >>= fun _ ->
      parse_varname
      >>= fun arg_name ->
      spaces *> string "->"
      >>= fun _ -> spaces *> parse_expr >>| fun body -> Ast.Abs (arg_name, body)
    in
    let app =
      spaces *> (parens abstr <|> parse_varname)
      >>= fun func ->
      many1
        (spaces *> conde [ parse_number; parse_varname; parens parse_expr ]
         >>| fun arg -> arg)
      >>| fun l -> List.fold_left (fun x arg -> Ast.App (x, arg)) func l
    in
    let helper = conde [ app; parse_varname <|> parse_number; parens parse_expr ] in
    let mul_helper =
      helper
      >>= fun left ->
      many
        (mul
         <|> div
         >>= fun operation -> helper >>= fun rightop -> return (operation, rightop))
      >>| List.fold_left (fun l (op, r) -> Ast.Binexpr (op, l, r)) left
    in
    let sum_helper =
      mul_helper
      >>= fun left ->
      many
        (plus <|> minus >>= fun operation -> mul_helper >>| fun right -> operation, right)
      >>| fun l -> List.fold_left (fun l (op, r) -> Ast.Binexpr (op, l, r)) left l
    in
    let comp =
      sum_helper
      >>= fun left ->
      option
        left
        (conde [ eq; neq; bi; le ]
         >>= fun op -> sum_helper >>| fun right -> Ast.Binexpr (op, left, right))
    in
    let conditional =
      spaces *> string "if"
      >>= fun _ ->
      parse_expr
      >>= fun cond ->
      spaces *> string "then"
      >>= fun _ ->
      parse_expr
      >>= fun tbranch ->
      spaces *> string "else"
      >>= fun _ -> parse_expr >>| fun ebranch -> Ast.Ite (cond, tbranch, ebranch)
    in
    let letbind =
      spaces *> string "let"
      >>= fun _ ->
      option (Ast.Recflag false) (spaces *> string "rec" >>| fun _ -> Ast.Recflag true)
      >>= fun recbool ->
      parse_varname
      >>= fun name ->
      many parse_varname
      >>= fun args ->
      spaces *> string "="
      >>= fun _ ->
      spaces *> parse_expr
      >>= fun letexpr ->
      option
        None
        (spaces *> string "in"
         >>= fun _ -> spaces *> parse_expr >>| fun bound -> Some bound)
      >>| fun inexpr -> Ast.Let (recbool, name, args, letexpr, inexpr)
    in
    conde [ letbind; comp; conditional; abstr ])
;;

let parse str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse")
;;
