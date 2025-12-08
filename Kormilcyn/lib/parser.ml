(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let no_ws p =
  let* _ = spaces
  and+ payload = p
  and+ _ = spaces in
  return payload
;;

let varname =
  let* var =
    take_while1 (function
      | 'a' .. 'z' -> true
      | _ -> false)
  in
  match var with
  | "fun" | "if" | "then" | "else" | "let" | "in" -> fail "Name not permitted"
  | _ -> return var
;;

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; atom : dispatch -> string Ast.t Angstrom.t
  ; mul_div : dispatch -> string Ast.t Angstrom.t
  ; add_sub : dispatch -> string Ast.t Angstrom.t
  }

let chainl1 p op =
  let rec loop acc =
    (let* f = op
     and+ y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let rec parse_fun pack =
  let* var = no_ws varname
  and+ body = no_ws (parse_fun pack) <|> no_ws (string "->") *> pack.add_sub pack in
  return (Ast.Fun (var, body))
;;

(* TODO: negation parsing *)
(* TODO: meainingful errors *)
let parse_miniml =
  let atom pack =
    fix (fun _ ->
      choice
        [ (* выражение в скобках *)
          (let* _ = no_ws (char '(')
           and+ tokens = pack.add_sub pack
           and+ _ = no_ws (char ')') <?> "Parentheses expected" in
           return tokens)
          (* функция *)
        ; (let* _ = no_ws (string "fun") in
           fix (fun body ->
             let* var = no_ws varname
             and+ b = no_ws body <|> no_ws (string "->") *> pack.add_sub pack in
             return (Ast.Fun (var, b))))
          (* if *)
        ; (let* _ = no_ws (string "if")
           and+ cond = pack.add_sub pack
           and+ _ = no_ws (string "then")
           and+ e1 = pack.add_sub pack
           and+ _ = no_ws (string "else")
           and+ e2 = pack.add_sub pack in
           return (Ast.If (cond, e1, e2)))
          (* let *)
        ; (let* _ = no_ws (string "let")
           and+ var = no_ws varname
           and+ _ = no_ws (char '=')
           and+ e1 = pack.add_sub pack
           and+ _ = no_ws (string "in")
           and+ e2 = pack.add_sub pack in
           return (Ast.Let (var, e1, e2)))
          (* переменная *)
        ; (let* s = no_ws varname in
           return (Ast.Var s))
          (* целое число *)
        ; (let* i = no_ws integer in
           return (Ast.Int (int_of_string i)))
        ])
  and apps pack =
    let* app =
      many1
        (let* token = no_ws (pack.atom pack) in
         return token)
    in
    match app with
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  and mul_div pack =
    let op =
      choice
        [ no_ws (char '*') *> return (fun l r -> Ast.Bin (Ast.Mul, l, r))
        ; no_ws (char '/') *> return (fun l r -> Ast.Bin (Ast.Div, l, r))
        ]
    in
    chainl1 (pack.apps pack) op
  and add_sub pack =
    let op =
      choice
        [ no_ws (char '+') *> return (fun l r -> Ast.Bin (Ast.Add, l, r))
        ; no_ws (char '-') *> return (fun l r -> Ast.Bin (Ast.Sub, l, r))
        ]
    in
    chainl1 (pack.mul_div pack) op
  in
  { atom; apps; mul_div; add_sub }
;;

let parse str =
  match
    Angstrom.parse_string
      (parse_miniml.add_sub parse_miniml)
      ~consume:Angstrom.Consume.All
      str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
