[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

type error = [ `parse_error of string ]

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
  | "let" | "in" | "fun" | "true" | "false" | "rec" | "else" | "if" | "then" | "_" | "fix"
    -> true
  | _ -> false
;;

let integer =
  spaces
  *> (take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
      >>= fun s -> return (Int (int_of_string s)))
  <|> (char '-'
       *> take_while1 (function
         | '0' .. '9' -> true
         | _ -> false)
       >>= fun s -> return (Int (-int_of_string s)))
;;

let is_first = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_other = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let identifier =
  spaces *> peek_char_fail
  >>= fun first_char ->
  if is_first first_char
  then
    take_while1 is_other
    >>= fun s -> if is_keyword s then fail "is keyword" else return (Var s)
  else fail "invalid identifier"
;;

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

(* *_* *)
let atom expr = choice [ integer; identifier; fun_expr expr; parens expr ]

(* sugar: f x y *)
let application expr =
  atom expr
  >>= fun f ->
  many (atom expr)
  >>= fun args -> return (List.fold_left (fun acc a -> App (acc, a)) f args)
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

(* let Надо подумать, возможно косяк*)
let let_expr expr =
  let without_in =
    spaces *> kw_let *> (spaces *> kw_rec *> return Rec <|> return NonRec)
    >>= fun rec_flag ->
    spaces *> identifier
    >>= fun name ->
    char '=' *> spaces *> expr
    >>= fun bound_expr ->
    match name with
    | Var name_str -> return (Let (rec_flag, name_str, bound_expr, None))
    | _ -> fail "Expected variable name"
  in
  let with_in =
    spaces *> kw_let *> (spaces *> kw_rec *> return Rec <|> return NonRec)
    >>= fun rec_flag ->
    spaces *> identifier
    >>= fun name ->
    char '=' *> spaces *> expr
    >>= fun bound_expr ->
    kw_in *> expr
    >>= fun body ->
    match name with
    | Var name_str -> return (Let (rec_flag, name_str, bound_expr, Some body))
    | _ -> fail "Expected variable name"
  in
  choice [ with_in; without_in ]
;;

(* op - подумать *)
let bin_ops expr =
  let make_chain next ops =
    next
    >>= fun first ->
    many (ops >>= fun op -> next >>= fun second -> return (op, second))
    >>= fun rest ->
    return (List.fold_left (fun acc (o, e) -> BinOp (o, acc, e)) first rest)
  in
  let app = application expr in
  (* rec? *)
  let mult_div =
    make_chain
      app
      (choice [ spaces *> string "*" *> return Mult; spaces *> string "/" *> return Div ])
  in
  let add_sub =
    make_chain
      mult_div
      (choice
         [ spaces *> string "+" *> return Plus; spaces *> string "-" *> return Minus ])
  in
  let compare =
    make_chain
      add_sub
      (choice
         [ string ">=" *> return EMore
         ; string "<=" *> return ELess
         ; string "=" *> return Equal
         ; string ">" *> return More
         ; string "<" *> return Less
         ])
  in
  compare
;;

(* seq *)
let seq_expr expr =
  sep_by1 (char ';' *> spaces) (bin_ops expr)
  >>= function
  | [ e ] -> return e
  | lst -> return (Seq lst)
;;

let fix_expr expr = kw_fix *> expr >>= fun e -> return (Fix e)

let expr =
  (* Возможно нужно сделать rec *)
  fix (fun expr ->
    choice
      [ seq_expr expr
      ; let_expr expr
      ; if_expr expr
      ; fun_expr expr
      ; fix_expr expr
      ; bin_ops expr
      ])
;;

let top = spaces *> expr <* spaces <* end_of_input (*костыль*)

let parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All top str with
  | Result.Ok x -> Result.Ok x
  | Error err -> Result.Error (`parse_error err)
;;
