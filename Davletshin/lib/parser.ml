(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom

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
  char '-' >>= (fun _ -> integer >>= fun n -> return (-n)) <|> integer
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.apps pack <* char ')' <?> "Parentheses expected"
        ; ((string "λ" <|> string "\\") *> spaces *> varname
           <* spaces
           <* (return () <* char '.' <|> string "->" *> return ())
           >>= fun var -> pack.apps pack >>= fun b -> return (Ast.Abs (var, b)))
        ; (varname <* spaces >>= fun var -> return (Ast.Var var))
        ; (number <* spaces >>= fun n -> return (Ast.Int n))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  in
  { single; apps }
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
