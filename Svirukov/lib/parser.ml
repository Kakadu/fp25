(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast

let is_keyword = function 
  | "let" | "in" | "fun" -> true
  | _ -> false


let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_spaces = skip_while is_space

let varname =
  function
    | 'a' .. 'z' | 'A'..'Z' | '_' -> true
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

let variable = token (take_while varname) >>| fun s -> if is_keyword s then s else raise (Invalid_argument "Keyword!!!")

let skip_parens p = token (char '(') *> p <* token (char ')')

let parse_bool = token (string "true") <|> token (string "false") >>| fun b -> if b = "true" then true else false

let is_digit = function 
| '0'..'9' -> true
| _ -> false

let cmp = choice [
  token (string ">=");
  token (string "<=");
  token (string "=");
  token (string "<");
  token (string ">");
]

let plus = token (char '+')
let minus = token (char '-')
let asteriks = token (char '*')

let dash = token (char '/')

let number =
  let integer = take_while1 is_digit >>| int_of_string in
  token (
    option '+' (char '-') >>= fun sign ->
    integer >>| fun num ->
    match sign with
    | '+' -> num
    | '-' -> -num
    | _ -> num
  )

let plus_op = plus *> return Plus
let minus_op = minus *> return Minus
let asterisk_op = asteriks *> return Asteriks
let slash_op = dash *> return Dash

let cmp_op =
  cmp >>= function
  | ">=" -> return EqMore
  | "<=" -> return EqLess
  | "=" -> return Equals
  | ">" -> return MoreThan
  | "<" -> return LessThan
  | _ -> fail "Invalid comparison operator"

let expr =
  fix (fun expr ->
    let atom =
      conde [
        number >>| (fun n -> Constant (CInt n));
        skip_parens expr;
      ]
    in
    let mul_div =
      atom >>= fun first ->
      many (
        conde [asterisk_op; slash_op] >>= fun op ->
        atom >>| fun right -> (op, right)
      ) >>| List.fold_left (fun left (op, right) -> Binop (op, left, right)) first
    in
    
    let add_sub =
      mul_div >>= fun first ->
      many (
        conde [plus_op; minus_op] >>= fun op ->
        mul_div >>| fun right -> (op, right)
      ) >>| List.fold_left (fun left (op, right) -> Binop (op, left, right)) first
    in
    
    let comparison =
      add_sub >>= fun left ->
      option left (
        cmp_op >>= fun op ->
        add_sub >>| fun right -> Binop (op, left, right)
      )
    in

    comparison
  )

let rec printer = function
| Constant (CInt n) -> Printf.sprintf "%d" n
| Binop (op, left, right) -> 
  let l = printer left in
  let r = printer right in
  let s = match op with 
  | Plus -> "+"
  | Minus -> "-"
  | Asteriks -> "*"
  | Dash -> "/"
  | Equals -> "="
  | MoreThan -> ">"
  | LessThan -> "<"
  | EqLess -> "<="
  | EqMore -> ">=" in
  Printf.sprintf "( %s %s %s)" l s r
| _ -> "Can not parse them yet"


let parse str =
  match
    Angstrom.parse_string (expr) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
