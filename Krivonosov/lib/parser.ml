(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

(* Keyword filtering *)
let is_keyword = function
  | "let" | "rec" | "if" | "then" | "else" | "in" | "fun" | "fix" -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

(* Multi-character identifier parser *)
let identifier =
  let first_char_parser =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  lift2
    (fun fc rest -> String.make 1 fc ^ rest)
    first_char_parser
    (take_while is_ident_char)
  >>= fun ident ->
  if is_keyword ident
  then fail ("Keyword '" ^ ident ^ "' cannot be used as identifier")
  else return ident
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

(* Integer literal parser *)
let pinteger =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
  >>| fun n -> Ast.Int n
;;

(* Main parser using single fix point *)
let pexpr =
  fix (fun pexpr ->
    (* Atomic expressions: integers, variables and parenthesized expressions *)
    let patom =
      spaces
      *> choice
           [ char '(' *> pexpr <* char ')' <* spaces <?> "Parentheses expected"
           ; pinteger <* spaces
           ; (identifier <* spaces >>| fun name -> Ast.Var name)
           ]
    in
    (* Lambda abstractions *)
    let plambda =
      (string "λ" <|> string "\\") *> spaces *> identifier
      <* spaces
      <* (char '.' <|> string "->" *> return ' ')
      <* spaces
      >>= fun param -> pexpr >>| fun body -> Ast.Abs (param, body)
    in
    (* Application: sequence of atoms *)
    let papp =
      many1 patom
      >>| function
      | [] -> failwith "impossible: many1 returned empty list"
      | [ x ] -> x
      | x :: xs -> List.fold_left (fun acc e -> Ast.App (acc, e)) x xs
    in
    (* Top level: try lambda first, then application *)
    spaces *> (plambda <|> papp) <* spaces)
;;

let parse str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pexpr str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
