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
let space1 = skip is_space

let is_keyword = function
  | "let" | "rec" | "in" | "fun" | "if" | "then" | "else" -> true
  | _ -> false
;;

let first_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let not_first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
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

let number = take_while1 is_digit >>| int_of_string

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

let parser =
  fix (fun parser ->
    let single =
      conde
        [ (varname <* spaces >>= fun var -> return (Var var))
        ; (number <* spaces >>= fun n -> return (Int n))
        ; char '(' *> parser <* char ')'
        ; (string "if" *> parser
           >>= fun c ->
           spaces *> string "then" *> parser
           >>= fun t -> spaces *> string "else" *> parser >>| fun e -> If (c, t, e))
        ; (string "fun" *> spaces *> many1 (varname <* spaces)
           <* spaces
           <* string "->"
           >>= fun args -> parser >>= fun body -> return (desugar_abs args body))
        ; (string "let" *> option Nonrec (spaces *> string "rec" *> return Rec)
           >>= fun flag ->
           spaces *> varname
           >>= fun name ->
           many (spaces *> varname)
           >>= fun args ->
           spaces *> char '=' *> parser
           >>= fun bind ->
           spaces *> string "in" *> parser
           >>| fun body -> Let (flag, name, desugar_abs args bind, body))
        ]
    in
    let apps =
      many1 (spaces *> single)
      >>= function
      | [] -> fail "bad syntax"
      | x :: xs -> return @@ List.fold_left (fun l r -> App (l, r)) x xs
    in
    let unary =
      spaces *> (char '-' *> spaces *> apps >>| fun e -> Unop (Neg, e)) <|> apps
    in
    let multiplicative =
      chain_left
        unary
        (conde
           [ spaces *> char '*' *> return Times; spaces *> char '/' *> return Divide ])
    in
    let additive =
      chain_left
        multiplicative
        (conde [ spaces *> char '+' *> return Plus; spaces *> char '-' *> return Minus ])
    in
    let comparison =
      chain_left
        additive
        (conde
           [ spaces *> string "=" *> space1 *> return Eq
           ; spaces *> string "<>" *> space1 *> return Neq
           ; spaces *> string "<" *> space1 *> return Lt
           ; spaces *> string ">" *> space1 *> return Gt
           ; spaces *> string "<=" *> space1 *> return Le
           ; spaces *> string ">=" *> space1 *> return Ge
           ])
    in
    comparison)
;;

let parse str =
  match Angstrom.parse_string parser ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
