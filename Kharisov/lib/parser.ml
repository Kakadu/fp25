[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let ws = skip_while is_ws
let token p = ws *> p <* ws
let parens p = token (char '(') *> p <* token (char ')')
let symbol s = token (string s)

let keyword k =
  token
    (string k
     >>= fun _ ->
     peek_char
     >>= function
     | Some c when is_ident_char c -> fail "not a keyword boundary"
     | _ -> return ())
;;

let is_keyword = function
  | "let" | "rec" | "in" | "if" | "then" | "else" | "fun" | "true" | "false" -> true
  | _ -> false
;;

let ident =
  token
    (let* c =
       satisfy (function
         | 'a' .. 'z' | '_' -> true
         | _ -> false)
     in
     let* rest = take_while is_ident_char in
     let name = String.make 1 c ^ rest in
     if is_keyword name then fail "identifier is a keyword" else return name)
;;

let digits =
  token
    (let* s =
       take_while1 (function
         | '0' .. '9' -> true
         | _ -> false)
     in
     let* () =
       peek_char
       >>= function
       | Some c when is_ident_char c -> fail "digit followed by letter"
       | _ -> return ()
     in
     match int_of_string_opt s with
     | Some n -> return n
     | None -> fail "integer literal overflow")
;;

let integer = digits >>| fun n -> EConst n

(* stolen from Angstrom README, only God knows who could create that *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let expr =
  fix (fun expr ->
    let atom =
      choice
        [ parens expr
        ; keyword "true" *> return (EConst 1)
        ; keyword "false" *> return (EConst 0)
        ; integer
        ; (ident >>| fun x -> EVar x)
        ]
    in
    let app =
      let* f = atom in
      let* args = many atom in
      return (List.fold_left (fun acc a -> EApp (acc, a)) f args)
    in
    let unary =
      let neg_literal = digits >>| fun n -> EConst (-n) in
      let neg_expr = app >>| fun e -> EBinop (Sub, EConst 0, e) in
      token (char '-') *> (neg_literal <|> neg_expr) <|> app
    in
    let mul_div =
      let op =
        symbol "*" *> return (fun a b -> EBinop (Mul, a, b))
        <|> symbol "/" *> return (fun a b -> EBinop (Div, a, b))
      in
      chainl1 unary op
    in
    let add_sub =
      let op =
        symbol "+" *> return (fun a b -> EBinop (Add, a, b))
        <|> symbol "-" *> return (fun a b -> EBinop (Sub, a, b))
      in
      chainl1 mul_div op
    in
    let comparison =
      let* lhs = add_sub in
      let cmp_op =
        choice
          [ symbol "<>" *> return Neq
          ; symbol "<=" *> return Le
          ; symbol ">=" *> return Ge
          ; symbol "<" *> return Lt
          ; symbol ">" *> return Gt
          ; symbol "=" *> return Eq
          ]
      in
      (let* op = cmp_op in
       let* rhs = add_sub in
       return (EBinop (op, lhs, rhs)))
      <|> return lhs
    in
    let param = parens ident <|> ident in
    let fun_expr =
      let* _ = keyword "fun" in
      let* params = many1 param in
      let* _ = symbol "->" in
      let* body = expr in
      return (List.fold_right (fun p acc -> EFun (p, acc)) params body)
    in
    let let_expr =
      let* _ = keyword "let" in
      let* rf = keyword "rec" *> return Rec <|> return NonRec in
      let* name = parens ident <|> ident in
      let* params = many param in
      let* _ = symbol "=" in
      let* rhs = expr in
      let* _ = keyword "in" in
      let* body = expr in
      let rhs = List.fold_right (fun p acc -> EFun (p, acc)) params rhs in
      return (ELet (rf, name, rhs, body))
    in
    let if_expr =
      let* _ = keyword "if" in
      let* cond = expr in
      let* _ = keyword "then" in
      let* e1 = expr in
      let* _ = keyword "else" in
      let* e2 = expr in
      return (EIf (cond, e1, e2))
    in
    choice [ let_expr; if_expr; fun_expr; comparison ])
;;

type error = [ `Parse_error of string ]

let parse (s : string) : (expr, [> error ]) result =
  match
    Angstrom.parse_string ~consume:Consume.All (ws *> expr <* ws <* end_of_input) s
  with
  | Ok e -> Ok e
  | Error msg -> Error (`Parse_error msg)
;;
