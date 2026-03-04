[@@@ocaml.text "/*"]

(** Copyright 2026, [ChurinNick] *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast




let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false


let spaces = skip_while is_whitespace
let token p = spaces *> p <* spaces


let keywords = [
  "let"; "rec"; "in"; "if"; "then"; "else"; "fun"; "fix"
]


let ident =
  let* first = satisfy is_letter in
  let* rest = take_while (fun c -> is_letter c || is_digit c) in
  let name = String.make 1 first ^ rest in
  if List.mem name keywords then
    fail ("keyword '" ^ name ^ "' is reserved")
  else
    return name


let integer =
  let* sign = 
    peek_char >>= function
    | Some '-' -> advance 1 *> return (-1)
    | _ -> return 1
  in
  let* digits = take_while1 is_digit in
  match int_of_string_opt digits with
  | Some n -> return (sign * n)
  | None -> fail "integer literal out of range"


let parens p = char '(' *> spaces *> p <* char ')' <* spaces



let parse_expression =
  let expr_parser = fix (fun expr ->
    
    let atom = 
      choice [
        (token integer >>| fun n -> Lit (Integer n));
        (token (string "()") >>| fun _ -> Lit UnitVal);
        (token ident >>| fun name -> Var name);
        (parens expr)
      ]
    in

    
    let unary =
      (token (char '-') *> atom >>| fun e -> UnOp (Negate, e))
      <|> atom
    in

    
    let apply =
      unary >>= fun func ->
      many atom >>| fun args ->
      List.fold_left (fun acc arg -> App (acc, arg)) func args
    in

    
    let mult =
      let op = choice [
          (token (char '*') *> return Mul);
          (token (char '/') *> return Div)
        ]
      in
      let rec mult_parser acc =
        (op >>= fun op' ->
         apply >>= fun right ->
         mult_parser (BinOp (op', acc, right)))
        <|> return acc
      in
      apply >>= mult_parser
    in

    
    let add =
      let op = choice [
          (token (char '+') *> return Add);
          (token (char '-') *> return Sub)
        ]
      in
      let rec add_parser acc =
        (op >>= fun op' ->
         mult >>= fun right ->
         add_parser (BinOp (op', acc, right)))
        <|> return acc
      in
      mult >>= add_parser
    in

    
    let cmp =
      add >>= fun left ->
      let op = choice [
          (token (char '=') *> return Eq);
          (token (string "<>") *> return Neq);
          (token (string "<=") *> return Le);
          (token (string ">=") *> return Ge);
          (token (char '<') *> return Lt);
          (token (char '>') *> return Gt)
        ]
      in
      (op >>= fun op' ->
       add >>= fun right ->
       return (CmpOp (op', left, right)))
      <|> return left
    in

    
    let if_expr =
      token (string "if") *>
      expr >>= fun cond ->
      token (string "then") *>
      expr >>= fun then_branch ->
      token (string "else") *>
      expr >>| fun else_branch ->
      If (cond, then_branch, else_branch)
    in

    
    let let_expr =
      token (string "let") *>
      (token (string "rec") *> return Recursive <|> return Plain) >>= fun rec_flag ->
      token ident >>= fun name ->
      many (token ident) >>= fun params ->
      token (char '=') *>
      expr >>= fun bound ->
      token (string "in") *>
      expr >>| fun body ->
      
      
      let bound_with_lambdas = 
        List.fold_right (fun param acc -> Lam (param, acc)) params bound
      in
      Let (rec_flag, name, bound_with_lambdas, body)
    in

    
    let fix_expr =
      token (string "fix") *> atom >>= fun f ->
      many atom >>| fun args ->
      List.fold_left (fun acc arg -> App (acc, arg)) (Fix f) args
    in
    
    let lambda_expr =
      token (string "fun") *>
      many1 (token ident) >>= fun params ->
      token (string "->") *>
      expr >>| fun body ->
      
      List.fold_right (fun param acc -> Lam (param, acc)) params body
    in

    
    choice [
      let_expr;
      if_expr;
      fix_expr;
      lambda_expr;
      cmp 
    ]
  ) in
  spaces *> expr_parser <* spaces <* end_of_input



type error = [ `Parsing_error of string ]

let parse (source : string) : (expr, [> `Parsing_error of string ]) result =
  match Angstrom.parse_string ~consume:All parse_expression source with
  | Ok ast -> Ok ast
  | Error _ -> Error (`Parsing_error "Parsing error")