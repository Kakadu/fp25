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
  | "or"
  | "match"
  | "with"
  | "type"
  | "of" -> true
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

let identifier_raw =
  lift2 (fun h t -> String.make 1 h ^ t) (satisfy is_first) (take_while is_other)
  >>= fun s -> if is_keyword s then fail ("keyword: " ^ s) else return s
;;

(* let type_params = many (spaces *> char '\'' *> identifier) пока без реализации *)

let simple_keyword s =
  spaces *> string s
  <* (spaces1
      <|> (peek_char
           >>= function
           | Some '(' -> return ()
           | Some c when not (is_other c) -> return ()
           | _ -> fail "Invalid entry"))
;;

let type_keyword s =
  spaces *> string s
  <* (spaces1
      <|> (peek_char
           >>= function
           | None -> return ()
           | Some '(' -> return ()
           | Some c when not (is_other c) -> return ()
           | _ -> fail "Invalid entry"))
;;

let ident_expr =
  identifier
  >>= fun name ->
  match name.[0] with
  | 'A' .. 'Z' -> return (Constr name)
  | _ -> return (Var name)
;;

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
let kw_match = kw "match"
let kw_with = spaces *> string "with" <* spaces
let kw_bar = spaces *> char '|' <* spaces
let kw_type = kw "type"
let kw_of = kw "of"
let semicolon = spaces *> string ";;" <* spaces

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
    let ptuple =
      char '(' *> spaces *> sep_by1 comma pattern
      <* spaces
      <* char ')'
      >>| function
      | [ p ] -> p
      | ps -> PTuple ps
    in
    let p_wild = spaces *> char '_' *> return PWildcard in
    let p_ident =
      identifier
      >>= fun name ->
      match name.[0] with
      | 'A' .. 'Z' -> many pattern >>= fun args -> return (PConstr (name, args))
      | _ -> return (PVar name)
    in
    spaces *> choice [ ptuple; p_wild; p_ident ] <* spaces)
;;

let expr =
  fix (fun expr ->
    let fun_expr =
      let* params = kw_fun *> many1 pattern in
      kw_arrow *> expr
      >>| fun body -> List.fold_right (fun pat acc -> Abs (pat, acc)) params body
    in
    let atom =
      spaces *> choice [ integer; boolean; ident_expr; fun_expr; tuple_p expr ] <* spaces
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
    let match_case =
      let* _ = kw_bar in
      let* pat = pattern in
      let* _ = kw_arrow in
      let* expr = expr <* spaces in
      return (pat, expr)
    in
    let match_expr =
      let* scrutinee = kw_match *> expr in
      let* _ = kw_with in
      let* cases = many1 match_case in
      return (Match (scrutinee, cases))
    in
    choice [ let_expr; if_expr; match_expr; bin_ops ])
;;

let type_expr =
  fix (fun type_expr ->
    let type_arrow = spaces *> string "->" <* spaces in
    let type_atom =
      spaces
      *> choice
           [ type_keyword "int" *> return TEInt
           ; type_keyword "bool" *> return TEBool
           ; type_keyword "unit" *> return TEUnit
           ; (char '\'' *> identifier >>| fun v -> TEVar v)
           ; (identifier >>| fun name -> TEConstr (name, []))
           ; parens type_expr
           ]
      <* spaces
    in
    let paren_args =
      spaces
      *> char '('
      *> spaces
      *> sep_by1 comma type_expr
      <* spaces
      <* char ')'
      <* spaces
    in
    let type_args = paren_args <|> (type_atom >>| fun t -> [ t ]) in
    let type_app =
      let* args = type_args in
      let* ctors = many (identifier_raw <* spaces) in
      match args, ctors with
      | [ single ], [] -> return single
      | _, [] -> fail "type constructor expected"
      | args, ctor :: rest ->
        let applied = TEConstr (ctor, args) in
        return (List.fold_left (fun acc name -> TEConstr (name, [ acc ])) applied rest)
    in
    let* t1 = type_app in
    option t1 (type_arrow *> type_expr >>| fun t2 -> TEArrow (t1, t2)))
;;

let constr_decl =
  let* name = identifier in
  let* args = option [] (kw_of *> sep_by1 (spaces *> char '*' <* spaces) type_expr) in
  return { ctor_name = name; ctor_args = args }
;;

let type_decl =
  let* _ = kw_type in
  let* params = many (spaces *> char '\'' *> identifier) in
  let* name = identifier in
  let* _ = spaces *> char '=' <* spaces in
  let* _ = option () (spaces *> char '|' *> spaces) in
  let* ctors = sep_by1 (spaces *> char '|' <* spaces) constr_decl in
  return { type_name = name; type_params = params; constructors = ctors }
;;

let toplevel =
  spaces *> choice [ (type_decl >>| fun td -> TLType td); (expr >>| fun e -> TLExpr e) ]
  <* spaces
;;

let prog =
  let* tl = sep_by semicolon toplevel in
  let* _ = many (spaces *> string ";;" <* spaces) in
  end_of_input *> return tl
;;

(* let top = toplevel *)
(* spaces *> expr <* spaces <* end_of_input *)

(* let toplevel = spaces *> (expr >>| fun e -> TLExpr e) <* spaces <* end_of_input *)
let parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All prog str with
  | Result.Ok x -> Result.Ok x
  | Error err -> Result.Error (`parse_error err)
;;
