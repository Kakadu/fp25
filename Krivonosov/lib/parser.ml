[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

(* Keyword filtering *)
let is_keyword = function
  | "let" | "rec" | "if" | "then" | "else" | "in" | "fun" -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

(* Word boundary — next char must not be part of an identifier *)
let word_boundary =
  peek_char
  >>= function
  | Some c when is_ident_char c -> fail "expected word boundary"
  | _ -> return ()
;;

(* Keyword parser: matches exact word and skips trailing spaces *)
let keyword w = string w *> word_boundary *> spaces

(* Multi-character identifier parser *)
let identifier =
  let first_char_parser =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  lift2
    (fun fc rest -> String.make 1 fc ^ rest)
    first_char_parser
    (take_while is_ident_char)
  >>= fun ident ->
  if is_keyword ident
  then fail ("Keyword '" ^ ident ^ "' cannot be used as identifier")
  else return ident
;;

type error = Parsing_error of string

let pp_error ppf = function
  | Parsing_error s -> Format.fprintf ppf "%s" s
;;

(* Integer literal parser *)
let pinteger =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
  >>| fun n -> Ast.Int n
;;

(* Chainl1: left-associative infix operator parser *)
let chainl1 p op =
  let rec go acc = op >>= (fun f -> p >>= fun x -> go (f acc x)) <|> return acc in
  p >>= go
;;

(* Main parser using single fix point *)
let pexpr =
  fix (fun pexpr ->
    (* Atomic expressions: integers, variables and parenthesized expressions *)
    let patom =
      spaces
      *> choice
           [ char '(' *> pexpr <* char ')' <* spaces <?> "Parentheses expected"
           ; pinteger <* spaces
           ; (identifier <* spaces >>| fun name -> Ast.Var name)
           ]
    in
    (* Lambda abstractions: fun x y z -> body *)
    let plambda =
      keyword "fun" *> many1 (identifier <* spaces)
      <* string "->"
      <* spaces
      >>= fun params ->
      pexpr
      >>| fun body ->
      (* Desugar: fun x y z -> body becomes fun x -> fun y -> fun z -> body *)
      List.fold_right (fun param acc -> Ast.Abs (param, acc)) params body
    in
    (* If-then-else expression *)
    let pif =
      keyword "if" *> pexpr
      >>= fun cond ->
      spaces *> keyword "then" *> pexpr
      >>= fun then_branch ->
      option None (spaces *> keyword "else" *> pexpr >>| Option.some)
      >>| fun else_branch -> Ast.If (cond, then_branch, else_branch)
    in
    (* Let expression *)
    let plet =
      keyword "let" *> option false (keyword "rec" *> return true)
      >>= fun is_rec ->
      identifier
      <* spaces
      <* char '='
      <* spaces
      >>= fun name ->
      pexpr
      >>= fun binding ->
      spaces *> keyword "in" *> pexpr >>| fun body -> Ast.Let (is_rec, name, binding, body)
    in
    (* Application: sequence of atoms *)
    let papp =
      patom
      >>= fun first ->
      many patom >>| fun rest -> List.fold_left (fun acc e -> Ast.App (acc, e)) first rest
    in
    (* Binary operators with precedence *)
    (* Comparison operators: =, <>, <, >, <=, >= (lowest precedence) *)
    let pcmp_op =
      spaces
      *> choice
           [ string "<=" *> return (fun l r -> Ast.BinOp (Ast.Leq, l, r))
           ; string ">=" *> return (fun l r -> Ast.BinOp (Ast.Geq, l, r))
           ; string "<>" *> return (fun l r -> Ast.BinOp (Ast.Neq, l, r))
           ; char '<' *> return (fun l r -> Ast.BinOp (Ast.Lt, l, r))
           ; char '>' *> return (fun l r -> Ast.BinOp (Ast.Gt, l, r))
           ; char '=' *> return (fun l r -> Ast.BinOp (Ast.Eq, l, r))
           ]
      <* spaces
    in
    (* Additive operators: +, - *)
    let padd_op =
      spaces
      *> choice
           [ char '+' *> return (fun l r -> Ast.BinOp (Ast.Add, l, r))
           ; char '-' *> return (fun l r -> Ast.BinOp (Ast.Sub, l, r))
           ]
      <* spaces
    in
    (* Multiplicative operators: *, /, % (higher precedence) *)
    let pmul_op =
      spaces
      *> choice
           [ char '*' *> return (fun l r -> Ast.BinOp (Ast.Mul, l, r))
           ; char '/' *> return (fun l r -> Ast.BinOp (Ast.Div, l, r))
           ; char '%' *> return (fun l r -> Ast.BinOp (Ast.Mod, l, r))
           ]
      <* spaces
    in
    (* Precedence levels: mul > add > cmp > app > lambda *)
    let pmul = chainl1 papp pmul_op in
    let padd = chainl1 pmul padd_op in
    let pcmp = chainl1 padd pcmp_op in
    (* Top level: try let, if, lambda first, then comparison *)
    spaces *> choice [ plet; pif; plambda; pcmp ] <* spaces)
;;

let parse str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pexpr str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (Parsing_error er)
;;
