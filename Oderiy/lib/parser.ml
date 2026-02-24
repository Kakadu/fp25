(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_whitespace = function
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | '\r' -> true
  | _ -> false
;;

let ws = skip_while is_whitespace
let ws1 = take_while1 is_whitespace *> return ()
let token p = ws *> p
let token_ws p = ws1 *> p

let parse_ident =
  let* first_char =
    satisfy (function
      | 'a' .. 'z' -> true
      | '_' -> true
      | _ -> false)
  in
  let* rest =
    take_while (function
      | 'a' .. 'z' -> true
      | 'A' .. 'Z' -> true
      | '0' .. '9' -> true
      | '_' -> true
      | '\'' -> true
      | _ -> false)
  in
  let id = String.make 1 first_char ^ rest in
  match id with
  | "let" -> fail "reserved keyword"
  | "rec" -> fail "reserved keyword"
  | "in" -> fail "reserved keyword"
  | "if" -> fail "reserved keyword"
  | "then" -> fail "reserved keyword"
  | "else" -> fail "reserved keyword"
  | "fun" -> fail "reserved keyword"
  | _ -> return id
;;

let parse_int =
  let* digits =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  if String.length digits > 1 && digits.[0] = '0'
  then fail "leading zero"
  else
    let* next_char = peek_char in
    match next_char with
    | Some 'a' .. 'z' -> fail "identifier cannot start with digit"
    | Some 'A' .. 'Z' -> fail "identifier cannot start with digit"
    | Some '_' -> fail "identifier cannot start with digit"
    | Some '\'' -> fail "identifier cannot start with digit"
    | _ -> return (int_of_string digits)
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error msg -> Format.fprintf ppf "Parse error: %s" msg
;;

let left_assoc expr_parser op_parser =
  let rec loop left =
    op_parser
    >>= (fun make_binop -> expr_parser >>= fun right -> loop (make_binop left right))
    <|> return left
  in
  expr_parser >>= loop
;;

let make_fun params body = List.fold_right (fun p acc -> Ast.Fun (p, acc)) params body

type parsers =
  { p_expr : parsers -> string Ast.t t
  ; p_comp : parsers -> string Ast.t t
  ; p_add : parsers -> string Ast.t t
  ; p_mul : parsers -> string Ast.t t
  ; p_unary : parsers -> string Ast.t t
  ; p_app : parsers -> string Ast.t t
  ; p_primary : parsers -> string Ast.t t
  }

let miniml_parser =
  let p_primary self =
    fix (fun _ ->
      choice
        [ (token parse_int >>| fun n -> Ast.Int n)
        ; token (char '(') *> self.p_expr self <* token (char ')')
        ; (token (string "if") *> token_ws (self.p_expr self)
           >>= fun cond ->
           token (string "then") *> token_ws (self.p_expr self)
           >>= fun then_br ->
           token (string "else") *> token_ws (self.p_expr self)
           >>| fun else_br -> Ast.If (cond, then_br, else_br))
        ; (token (string "let") *> token_ws parse_ident
           >>= fun name ->
           many (token parse_ident)
           >>= fun params ->
           token (char '=') *> self.p_expr self
           >>= fun rhs ->
           token (string "in") *> token_ws (self.p_expr self)
           >>| fun body -> Ast.Let (name, make_fun params rhs, body))
        ; (token (string "let") *> token_ws (string "rec") *> token_ws parse_ident
           >>= fun fname ->
           many (token parse_ident)
           >>= fun params ->
           token (char '=') *> self.p_expr self
           >>= fun fn_body ->
           token (string "in") *> token_ws (self.p_expr self)
           >>| fun body -> Ast.LetRec (fname, make_fun params fn_body, body))
        ; (token (string "fun") *> many1 (token parse_ident)
           >>= fun params ->
           token (string "->") *> self.p_expr self >>| fun body -> make_fun params body)
        ; token (string "fix") *> return Ast.Fix
        ; (token parse_ident >>| fun v -> Ast.Var v)
        ])
  in
  let p_app self =
    lift2
      (fun hd tl -> List.fold_left (fun f arg -> Ast.App (f, arg)) hd tl)
      (self.p_primary self)
      (many (self.p_primary self))
  in
  let p_unary self =
    fix (fun _ ->
      token (char '+') *> self.p_unary self
      <|> (token (char '-') *> self.p_unary self >>| fun e -> Ast.Neg e)
      <|> self.p_app self)
  in
  let p_mul self =
    let ops =
      choice
        [ token (char '*') *> return (fun l r -> Ast.Bin (Ast.Mul, l, r))
        ; token (char '/') *> return (fun l r -> Ast.Bin (Ast.Div, l, r))
        ]
    in
    left_assoc (self.p_unary self) ops
  in
  let p_add self =
    let ops =
      choice
        [ token (char '+') *> return (fun l r -> Ast.Bin (Ast.Add, l, r))
        ; token (char '-') *> return (fun l r -> Ast.Bin (Ast.Sub, l, r))
        ]
    in
    left_assoc (self.p_mul self) ops
  in
  let p_comp self =
    let ops =
      choice
        [ token (string "<=") *> return (fun l r -> Ast.Bin (Ast.Leq, l, r))
        ; token (char '<') *> return (fun l r -> Ast.Bin (Ast.Lt, l, r))
        ; token (string ">=") *> return (fun l r -> Ast.Bin (Ast.Geq, l, r))
        ; token (char '>') *> return (fun l r -> Ast.Bin (Ast.Gt, l, r))
        ; token (char '=') *> return (fun l r -> Ast.Bin (Ast.Eq, l, r))
        ]
    in
    left_assoc (self.p_add self) ops
  in
  let p_expr self = self.p_comp self in
  { p_expr; p_comp; p_add; p_mul; p_unary; p_app; p_primary }
;;

let parse_lam = miniml_parser

let parse input_str =
  match
    parse_string ~consume:Consume.All (miniml_parser.p_expr miniml_parser) input_str
  with
  | Ok ast -> Ok ast
  | Error msg -> Error (`Parsing_error msg)
;;
