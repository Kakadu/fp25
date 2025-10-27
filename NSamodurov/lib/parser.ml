(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let alpha =
  satisfy (function
      | 'a' .. 'z' -> true
      | 'A' .. 'Z' -> true
      | _ -> false)
;;

let digit =
    satisfy (function
      | '0' .. '9' -> true
      | _ -> false)
;;

let string_of_char_list =
  fun list ->
  let buf = Buffer.create 1024 in
  List.iter (Buffer.add_char buf) list;
  Buffer.contents buf

let varname =
  alpha >>= fun h -> many (alpha <|> digit <|> (char '_')) >>= fun tl -> return @@ string_of_char_list (h::tl)
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
        ; ((string "fun") *> many1 (spaces *> varname) <*
           spaces <* (string "->") *> return ()
           >>= fun list ->
           pack.apps pack >>= fun b ->
           return (List.fold_right (fun x acc -> Ast.Abs (x, acc)) list b))
        ; (varname <* spaces >>= fun c -> return (Ast.Var c))
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
