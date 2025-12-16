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

let spaces1 = take_while1 is_space *> return ()
let parens p = char '(' *> spaces *> p <* spaces <* char ')'

(** This is a reserved word *)
let is_keyword = function
  | "let" | "in" | "fun" | "true" | "false" | "rec" | "else" | "if" | "then" | "_" -> true
  | _ -> false
;;

let integer =
  spaces
  *> (take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
      >>= fun s -> return (Int (int_of_string s)))
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
  spaces
  *> lift2 (fun h t -> String.make 1 h ^ t) (satisfy is_first) (take_while is_other)
  >>= fun s -> if is_keyword s then fail ("keyword: " ^ s) else return s
;;

let var = identifier >>= fun s -> return (Var s)
let kw s = spaces *> string s <* spaces1
let kw_let = kw "let"
let kw_in = kw "in"
let kw_fun = kw "fun"
let kw_if = kw "if"
let kw_then = kw "then"
let kw_else = kw "else"
let kw_rec = kw "rec"

let expr =
  fix (fun expr ->
    let fun_expr =
      kw_fun *> many1 var
      >>= fun params ->
      kw "->" *> expr
      >>| fun body -> List.fold_right (fun arg f -> Abs (arg, f)) params body
    in
    let atom = spaces *> choice [ integer; var; fun_expr; parens expr ] <* spaces in
    let application =
      atom
      >>= fun f ->
      many atom
      >>= fun args ->
      match args with
      | [] -> return f
      | _ -> return (List.fold_left (fun acc a -> App (acc, a)) f args)
    in
    let unary =
      let neg = char '-' *> spaces *> application >>| fun x -> BinOp (Minus, Int 0, x) in
      neg <|> application
    in
    let let_expr =
      let rec_flag = spaces *> kw_rec *> return Rec <|> return NonRec in
      let make_without_in =
        kw_let *> rec_flag
        >>= fun rf ->
        identifier
        >>= fun name ->
        many (spaces *> identifier)
        >>= fun args ->
        spaces *> char '=' *> spaces *> expr
        >>= fun bound_expr ->
        let fun_expr =
          List.fold_right (fun arg acc -> Abs (Var arg, acc)) args bound_expr
        in
        return (Let (rf, name, fun_expr, None))
      in
      let make_with_in =
        kw_let *> rec_flag
        >>= fun rf ->
        identifier
        >>= fun name ->
        many (spaces *> identifier)
        >>= fun args ->
        spaces *> char '=' *> spaces *> expr
        >>= fun bound_expr ->
        kw_in *> expr
        >>= fun body ->
        let fun_expr =
          List.fold_right (fun arg acc -> Abs (Var arg, acc)) args bound_expr
        in
        return (Let (rf, name, fun_expr, Some body))
      in
      choice [ make_with_in; make_without_in ]
    in
    let if_expr =
      kw_if *> expr
      >>= fun cond ->
      kw_then *> expr
      >>= fun t ->
      kw_else *> expr >>| (fun e -> If (cond, t, Some e)) <|> return (If (cond, t, None))
    in
    let bin_ops =
      let make_chain next ops =
        next
        >>= fun first ->
        many (ops >>= fun op -> next >>= fun second -> return (op, second))
        >>| fun rest -> List.fold_left (fun acc (o, e) -> BinOp (o, acc, e)) first rest
      in
      let mult_div =
        make_chain
          unary
          (choice
             [ spaces *> string "*" *> return Mult; spaces *> string "/" *> return Div ])
      in
      let add_sub =
        make_chain
          mult_div
          (choice
             [ spaces *> string "+" *> return Plus; spaces *> string "-" *> return Minus ])
      in
      make_chain
        add_sub
        (choice
           [ spaces *> string ">=" *> return EMore
           ; spaces *> string "<=" *> return ELess
           ; spaces *> string "=" *> return Equal
           ; spaces *> string ">" *> return More
           ; spaces *> string "<" *> return Less
           ])
    in
    choice [ let_expr; if_expr; bin_ops ])
;;

let top = spaces *> expr <* spaces <* end_of_input

let parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All top str with
  | Result.Ok x -> Result.Ok x
  | Error err -> Result.Error (`parse_error err)
;;
