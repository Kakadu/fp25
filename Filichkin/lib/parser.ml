[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

(** Function to check for whitespace characters *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(** Skips any number of spaces *)
let spaces = skip_while is_space

(** Requires at least one space *)
let spaces1 = take_while1 is_space >>= fun _ -> return ()

let parens p = char '(' *> spaces *> p <* spaces <* char ')'
let braces p = char '{' *> spaces *> p <* spaces <* char '}'

(** This is a reserved word *)
let is_keyword = function
  | "let" | "in" | "fun" | "true" | "false" | "rec" | "else" | "if" | "then" | "_" -> true
  | _ -> false
;;

let integer =
  spaces
  *> take_while1 (function
    | '1' .. '9' -> true
    | _ -> false)
  >>= fun s -> return (Int (int_of_string s))
;;

let identifier =
  spaces
  *> take_while1 (function
    | 'a' .. 'z' -> true
    | _ -> false)
  >>= fun s -> if is_keyword s then fail "is keyword" else return (Var s)
;;

(* let keyword_parsers () =
   let kw s = spaces *> string s <* spaces in
   let let_ = kw "let"
   and in_ = kw "in"
   and fun_ = kw "fun"
   and if_ = kw "if"
   and then_ = kw "then"
   and else_ = kw "else"
   and rec_ = kw "rec"
   and fix_ = kw "fix" in
   let_, in_, fun_, if_, then_, else_, rec_, fix_
   ;; *)

let kw s = spaces *> string s <* spaces
let kw_let = kw "let"
let kw_in = kw "in"
let kw_fun = kw "fun"
let kw_if = kw "if"
let kw_then = kw "then"
let kw_else = kw "else"
let kw_rec = kw "rec"
let kw_fix = kw "fix"

let op =
  spaces
  *> choice
       [ string "+" *> return Plus
       ; string "-" *> return Minus
       ; string "*" *> return Mult
       ; string "/" *> return Div
       ; string "=" *> return Equal
       ; string ">" *> return More
       ; string "<" *> return Less
       ; string ">=" *> return EMore
       ; string "<=" *> return ELess
       ]
;;

(* *_* *)
let atom expr = choice [ integer; identifier; parens expr ]

(* sugar: f x y *)
let application expr =
  atom expr
  >>= fun f ->
  many1 (atom expr)
  >>= fun args -> return (List.fold_left (fun acc a -> App (acc, a)) f args) <|> atom expr
;;

(* fun *)
let fun_expr expr =
  kw_fun *> many1 identifier
  <* kw "->"
  >>= fun params ->
  expr
  >>= fun budy ->
  return
    (Abs
       ( List.map
           (function
             | Var x -> x
             | _ -> failwith "impossible")
           params
       , budy ))
;;

(* if *)
let if_expr expr =
  kw_if *> expr
  >>= fun cond ->
  kw_then *> expr
  >>= fun t ->
  kw_else *> expr
  >>= (fun e -> return (If (cond, t, Some e)))
  <|> return (If (cond, t, None))
;;
