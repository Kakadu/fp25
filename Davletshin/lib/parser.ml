(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let is_keyword = function
  | "let" | "rec" | "in" | "fun" | "if" | "then" | "else" -> true
  | _ -> false
;;

let first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let not_first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let varname =
  satisfy first_letter
  >>= fun first_c ->
  take_while not_first_letter
  >>= fun rest ->
  let name = String.make 1 first_c ^ rest in
  if is_keyword name then fail "keyword used as an identifier" else return name
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let number =
  let integer = take_while1 is_digit >>| int_of_string in
  (*char '-' >>= (fun _ -> integer >>= fun n -> return (-n)) <|>*) integer
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  ; multiplicative : dispatch -> string Ast.t Angstrom.t
  ; additive : dispatch -> string Ast.t Angstrom.t
  ; unary : dispatch -> string Ast.t Angstrom.t
  ; comparison : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let chain_left term op =
  term
  >>= fun first ->
  many (op >>= fun o -> term >>| fun right -> o, right)
  >>| fun rest ->
  List.fold_left (fun left (o, right) -> Binop (o, left, right)) first rest
;;

let desugar_abs args body = List.fold_right (fun arg acc -> Abs (arg, acc)) args body

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.comparison pack <* char ')' <?> "Parentheses expected"
        ; (string "fun" *> spaces *> many1 (varname <* spaces)
           <* spaces
           <* string "->"
           >>= fun args ->
           pack.comparison pack >>= fun body -> return (desugar_abs args body))
        ; (varname <* spaces >>= fun var -> return (Var var))
        ; (number <* spaces >>= fun n -> return (Int n))
        ; (spaces *> string "if" *> pack.comparison pack
           >>= fun c ->
           spaces *> string "then" *> pack.comparison pack
           >>= fun t ->
           spaces *> string "else" *> pack.comparison pack >>| fun e -> If (c, t, e))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> App (l, r)) x xs
  in
  let multiplicative pack =
    chain_left
      (apps pack)
      (conde [ spaces *> char '*' *> return Times; spaces *> char '/' *> return Divide ])
  in
  let additive pack =
    chain_left
      (multiplicative pack)
      (conde [ spaces *> char '+' *> return Plus; spaces *> char '-' *> return Minus ])
  in
  let unary pack =
    spaces *> char '-' *> spaces *> pack.additive pack
    >>| (fun e -> Unop (Neg, e))
    <|> pack.additive pack
  in
  let comparison pack =
    chain_left
      (unary pack)
      (conde
         [ spaces *> string "=" *> return Eq
         ; spaces *> string "<>" *> return Neq
         ; spaces *> string "<" *> return Lt
         ; spaces *> string ">" *> return Gt
         ; spaces *> string ">=" *> return Le
         ; spaces *> string ">=" *> return Ge
         ])
  in
  { single; apps; multiplicative; additive; unary; comparison }
;;

let parse str =
  match
    Angstrom.parse_string
      (parse_lam.comparison parse_lam)
      ~consume:Angstrom.Consume.All
      str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
