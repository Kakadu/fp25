(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
  | "fun" | "if" | "then" | "else" | "let" | "in" | "rec" -> fail "Name not permitted"
  | _ -> return var
;;

let integer =
  let* i =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  if String.length i > 1 && String.starts_with ~prefix:"0" i
  then fail "Incorrect integer"
  else return i
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; atom : dispatch -> string Ast.t Angstrom.t
  ; unary : dispatch -> string Ast.t Angstrom.t
  ; mul_div : dispatch -> string Ast.t Angstrom.t
  ; add_sub : dispatch -> string Ast.t Angstrom.t
  ; comp : dispatch -> string Ast.t Angstrom.t
  }

let chainl1 p op =
  let rec iter acc =
    (let* f = op
     and+ y = p in
     iter (f acc y))
    <|> return acc
  in
  let* x = p in
  iter x
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse_miniml =
  let atom pack =
    fix (fun _ ->
      choice
        [ (* выражение в скобках *)
          (let* _ = no_ws (char '(')
           and+ e = pack.comp pack
           and+ _ = no_ws (char ')') in
           return e)
          (* функция *)
        ; (let* _ = no_ws (string "fun") in
           fix (fun body ->
             let* var = no_ws varname
             and+ b = no_ws body <|> no_ws (string "->") *> pack.comp pack in
             return (Ast.Fun (var, b))))
          (* if *)
        ; (let* _ = no_ws (string "if")
           and+ cond = pack.comp pack
           and+ _ = no_ws (string "then")
           and+ e1 = pack.comp pack
           and+ _ = no_ws (string "else")
           and+ e2 = pack.comp pack in
           return (Ast.If (cond, e1, e2)))
          (* let *)
        ; (let* _ = no_ws (string "let")
           and+ var = no_ws varname
           and+ _ = no_ws (char '=')
           and+ e1 = pack.comp pack
           and+ _ = no_ws (string "in")
           and+ e2 = pack.comp pack in
           return (Ast.Let (var, e1, e2)))
          (* let rec *)
        ; (let* _ = no_ws (string "let")
           and+ _ = no_ws (string "rec")
           and+ fvar = no_ws varname
           and+ _ = no_ws (char '=')
           and+ f = pack.atom pack
           and+ _ = no_ws (string "in")
           and+ e = pack.comp pack in
           return (Ast.LetRec (fvar, f, e)))
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
        (let* e = no_ws (pack.atom pack) in
         return e)
    in
    match app with
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  and unary pack =
    fix (fun _ ->
      (* унарный плюс *)
      (let* _ = no_ws (char '+')
       and+ e = pack.unary pack in
       return e)
      <|>
      (* унарный минус *)
      (let* _ = no_ws (char '-')
       and+ e = pack.unary pack in
       return (Ast.Neg e))
      <|>
      let* e = pack.apps pack in
      return e)
  and mul_div pack =
    let op =
      choice
        [ no_ws (char '*') *> return (fun l r -> Ast.Bin (Ast.Mul, l, r))
        ; no_ws (char '/') *> return (fun l r -> Ast.Bin (Ast.Div, l, r))
        ]
    in
    chainl1 (pack.unary pack) op
  and add_sub pack =
    let op =
      choice
        [ no_ws (char '+') *> return (fun l r -> Ast.Bin (Ast.Add, l, r))
        ; no_ws (char '-') *> return (fun l r -> Ast.Bin (Ast.Sub, l, r))
        ]
    in
    chainl1 (pack.mul_div pack) op
  and comp pack =
    let op =
      choice
        [ no_ws (string "<=") *> return (fun l r -> Ast.Bin (Ast.Leq, l, r))
        ; no_ws (char '=') *> return (fun l r -> Ast.Bin (Ast.Eq, l, r))
        ; no_ws (string ">=") *> return (fun l r -> Ast.Bin (Ast.Geq, l, r))
        ]
    in
    chainl1 (pack.add_sub pack) op
  in
  { atom; apps; unary; mul_div; add_sub; comp }
;;

let parse str =
  match
    Angstrom.parse_string
      (parse_miniml.comp parse_miniml)
      ~consume:Angstrom.Consume.All
      str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
