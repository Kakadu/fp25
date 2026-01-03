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
  | "let"
  | "in"
  | "fun"
  | "true"
  | "false"
  | "rec"
  | "else"
  | "if"
  | "then"
  | "_"
  | "not"
  | "and"
  | "or" -> true
  | _ -> false
;;

let integer =
  spaces
  *> take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| fun digits -> Int (int_of_string digits)
;;

let boolean =
  spaces *> (string "true" *> return (Bool true))
  <|> string "false" *> return (Bool false)
  <* spaces
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

let simple_keyword s =
  spaces *> string s
  <* (spaces1
      <|> (peek_char
           >>= function
           | Some '(' -> return ()
           | Some c when not (is_other c) -> return ()
           | _ -> fail "Invalid entry"))
;;

let var = identifier >>= fun s -> return (Var s)
let kw s = spaces *> string s <* spaces1
let kw_let = kw "let"
let kw_in = simple_keyword "in"
let kw_fun = kw "fun"
let kw_if = simple_keyword "if"
let kw_then = simple_keyword "then"
let kw_else = simple_keyword "else"
let kw_rec = kw "rec"
let kw_arrow = simple_keyword "->"
let comma = spaces *> char ',' <* spaces

let tuple_p expr =
  char '(' *> spaces *> sep_by1 comma expr
  <* spaces
  <* char ')'
  >>| function
  | [ e ] -> e
  | es -> Tuple es
;;

let pattern =
  fix (fun pattern ->
    let pvar = identifier >>| fun x -> PVar x in
    let ptuple =
      char '(' *> spaces *> sep_by1 comma pattern
      <* spaces
      <* char ')'
      >>| function
      | [ p ] -> p
      | ps -> PTuple ps
    in
    spaces *> choice [ ptuple; pvar ] <* spaces)
;;

let expr =
  fix (fun expr ->
    let fun_expr =
      let* params = kw_fun *> many1 pattern in
      kw_arrow *> expr
      >>| fun body -> List.fold_right (fun pat acc -> Abs (pat, acc)) params body
    in
    let atom =
      spaces *> choice [ integer; boolean; var; fun_expr; tuple_p expr ] <* spaces
    in
    let unary_expr =
      fix (fun unary_expr ->
        let base = choice [ atom; parens unary_expr ] in
        choice
          [ (char '-' *> spaces *> base >>| fun e -> UnOp (Neg, e))
          ; (string "not" *> spaces1 *> base >>| fun e -> UnOp (Not, e))
          ; base
          ])
    in
    let application =
      let* f = unary_expr in
      let* args = many atom in
      match args with
      | [] -> return f
      | _ -> return (List.fold_left (fun acc a -> App (acc, a)) f args)
    in
    let let_expr =
      let rec_flag = kw_rec *> return Rec <|> return NonRec in
      let make with_in =
        let* rf = kw_let *> rec_flag in
        let* pat = pattern in
        let* args = many (spaces *> pattern) in
        let* bound_expr = spaces *> char '=' *> spaces *> expr in
        let fun_expr = List.fold_right (fun p acc -> Abs (p, acc)) args bound_expr in
        match with_in with
        | None -> return (Let (rf, pat, fun_expr, None))
        | Some () ->
          let* body = kw_in *> expr in
          return (Let (rf, pat, fun_expr, Some body))
      in
      choice [ make (Some ()); make None ]
    in
    let if_expr =
      let* cond = kw_if *> expr in
      let* t = kw_then *> expr in
      kw_else *> expr >>| fun e -> If (cond, t, e)
    in
    let bin_ops =
      let make_chain next ops =
        let* first = next in
        many
          (let* op = ops in
           let* second = next in
           return (op, second))
        >>| fun rest -> List.fold_left (fun acc (o, e) -> BinOp (o, acc, e)) first rest
      in
      let mult_div =
        make_chain
          application
          (choice
             [ spaces *> string "*" *> return Mult; spaces *> string "/" *> return Div ])
      in
      let add_sub =
        make_chain
          mult_div
          (choice
             [ spaces *> string "+" *> return Plus; spaces *> string "-" *> return Minus ])
      in
      let comp =
        make_chain
          add_sub
          (choice
             [ spaces *> string ">=" *> spaces *> return EMore
             ; spaces *> string "<=" *> spaces *> return ELess
             ; spaces *> string "<>" *> spaces *> return NotEqual
             ; spaces *> string "=" *> spaces *> return Equal
             ; spaces *> string ">" *> spaces *> return More
             ; spaces *> string "<" *> spaces *> return Less
             ])
      in
      let and_expr = make_chain comp (spaces *> string "&&" *> spaces *> return And) in
      let or_expr = make_chain and_expr (spaces *> string "||" *> spaces *> return Or) in
      or_expr
    in
    choice [ let_expr; if_expr; bin_ops ])
;;

let top = spaces *> expr <* spaces <* end_of_input

let parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All top str with
  | Result.Ok x -> Result.Ok x
  | Error err -> Result.Error (`parse_error err)
;;
