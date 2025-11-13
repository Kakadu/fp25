(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast

let with_error p =
  p
  >>= function
  | Ok result -> return result
  | Error (`Parsing_error msg) -> fail msg
;;

let is_keyword = function
  | "let" | "in" | "fun" | "true" | "false" | "rec" -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_spaces = skip_while is_space

let varname = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let token p = skip_spaces *> p

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let variable_name =
  token (take_while1 varname)
  >>| fun s ->
  if is_keyword s
  then raise (Invalid_argument "Keyword!!!")
  else if String.for_all is_digit s
  then raise (Invalid_argument "Varname can't be number")
  else s
;;

let pattern = variable_name >>| fun s -> if s = "_" then PAny else PVar s
let var = pattern >>| fun p -> Var p
let parse_bool = token (string "true") <|> token (string "false") >>| bool_of_string
let skip_parens p = token (char '(') *> p <* token (char ')')

let cmp =
  choice
    [ token (string ">=")
    ; token (string "<=")
    ; token (string "=")
    ; token (string "<")
    ; token (string ">")
    ]
;;

let number =
  let integer = take_while1 is_digit >>| int_of_string in
  token
    (option '+' (char '-')
     >>= fun sign ->
     integer
     >>| fun num ->
     match sign with
     | '+' -> num
     | '-' -> -num
     | _ -> num)
;;

let plus_op = token (char '+') *> return Plus
let minus_op = token (char '-') *> return Minus
let asterisk_op = token (char '*') *> return Asteriks
let slash_op = token (char '/') *> return Dash

let cmp_op =
  cmp
  >>= function
  | ">=" -> return EqMore
  | "<=" -> return EqLess
  | "=" -> return Equals
  | ">" -> return MoreThan
  | "<" -> return LessThan
  | _ -> fail "Invalid comparison operator"
;;

let expr =
  fix (fun expr ->
    let atom =
      fix (fun atom ->
        let app =
          var
          >>= fun func ->
          many1 atom
          >>| fun args -> List.fold_left (fun func arg -> App (func, arg)) func args
        in
        choice [ (number >>| fun n -> Constant (CInt n)); skip_parens expr; app; var ])
    in
    let binopr =
      let mul_div =
        atom
        >>= fun first ->
        many
          (conde [ asterisk_op; slash_op ] >>= fun op -> atom >>| fun right -> op, right)
        >>| List.fold_left (fun left (op, right) -> Binop (op, left, right)) first
      in
      let add_sub =
        mul_div
        >>= fun first ->
        many (conde [ plus_op; minus_op ] >>= fun op -> mul_div >>| fun right -> op, right)
        >>| List.fold_left (fun left (op, right) -> Binop (op, left, right)) first
      in
      let comparison =
        add_sub
        >>= fun left ->
        option left (cmp_op >>= fun op -> add_sub >>| fun right -> Binop (op, left, right))
      in
      comparison
    in
    let conditional =
      token (string "if") *> binopr
      >>= fun cond ->
      token (string "then") *> expr
      >>= fun main ->
      option None (token (string "else") *> expr >>| fun alt -> Some alt)
      >>= function
      | Some alt -> return (Conditional (cond, main, Some alt))
      | None -> return (Conditional (cond, main, None))
    in
    let let_binding =
      token (string "let") *> option NonRec (token (string "rec") >>| fun _ -> Rec)
      >>= fun recurs ->
      pattern
      >>= fun name ->
      many pattern
      >>= fun args ->
      token (char '=') *> expr
      >>= fun ex ->
      let rec build_curried_function args body =
        match args with
        | [] -> body
        | arg :: rest_args -> Fun (arg, build_curried_function rest_args body)
      in
      let body = if args = [] then ex else build_curried_function args ex in
      option None (token (string "in") *> expr >>| fun cont -> Some cont)
      >>= fun skope -> return (Let (recurs, name, body, skope))
    in
    choice [ let_binding; conditional; binopr ])
;;

let parse str =
  match Angstrom.parse_string expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
