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
  spaces
  >>= fun _ ->
  sign
  >>= fun sign ->
  take_while1 is_digit
  >>| fun x ->
  match sign with
  | "-" -> Ast.Const (-1 * int_of_string x)
  | _ -> Ast.Const (int_of_string x)
;;

let parse_varname =
  spaces
  >>= fun _ ->
  take_while1 var_name
  >>= fun s ->
  match s with
  | _ when String.for_all is_digit s -> fail "Number cant be a name"
  | _ -> return (Ast.Ident s)
;;

let plus = spaces >>= fun _ -> char '+' >>= fun _ -> return Plus
let minus = spaces >>= fun _ -> char '-' >>= fun _ -> return Minus
let mul = spaces >>= fun _ -> char '*' >>= fun _ -> return Mul
let div = spaces >>= fun _ -> char '/' >>= fun _ -> return Div
let eq = spaces >>= fun _ -> char '=' >>= fun _ -> return Eq
let neq = spaces >>= fun _ -> string "!=" >>= fun _ -> return Neq
let le = spaces >>= fun _ -> char '<' >>= fun _ -> return Le
let bi = spaces >>= fun _ -> char '>' >>= fun _ -> return Bi

let parse_expr =
  fix (fun parse_expr ->
    let helper = conde [ parse_varname <|> parse_number; parens parse_expr ] in
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
    comp)
;;

let parse str =
  match
    Angstrom.parse_string
      (parse_number <|> parse_varname)
      ~consume:Angstrom.Consume.All
      str
  with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse")
;;
