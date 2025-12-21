(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

(* Whitespace parser reused everywhere *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

(* Consume spaces around parser [p] *)
let token p = p <* spaces

(* Parse a fixed symbol, skipping spaces after it *)
let symbol s = token (string s)

(* Parenthesised expression *)
let parens p = symbol "(" *> p <* symbol ")"

(* Identifiers and integers *)

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false
;;

let ident_raw =
  satisfy is_ident_start
  >>= fun first ->
  take_while is_ident_char
  >>| fun rest -> String.make 1 first ^ rest
;;

(* List of keywords that cannot be used as variable names *)
let keywords = [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else" ]

(* [ident] parses variable and function names, rejecting keywords *)
let ident =
  token ident_raw
  >>= fun s ->
  if List.mem s keywords
  then fail "Keyword cannot be used as identifier"
  else return s
;;

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| int_of_string
  |> token

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

(* Main expression parser for miniML *)
let expr : string Ast.t Angstrom.t =
  let open Ast in
  fix (fun expr ->
    (* [chainl1 p op] parses left associative binary operators like [a + b + c] *)
    let chainl1 p op =
      p
      >>= fun init ->
      many (op >>= fun f -> p >>| fun v -> (f, v))
      >>| fun rest ->
      List.fold_left (fun acc (f, v) -> f acc v) init rest
    in

    (* Atomic expressions: integers, variables and parenthesised expressions *)
    let atom =
      choice
        [ parens expr
        ; (integer >>| fun n -> Int n)
        ; (ident >>| fun x -> Var x)
        ]
    in

    (* Function application is left associative and tighter than any infix operator *)
    let app =
      many1 atom
      >>| function
      | [ x ] -> x
      | x :: xs -> List.fold_left (fun acc r -> App (acc, r)) x xs
      | [] -> failwith "application on empty list, impossible by [many1]"
    in

    let unary =
      fix (fun self ->
        choice
          [ (symbol "-" *> self >>| function
             | Int n -> Int (-n)
             | e -> Binop (Sub, Int 0, e))
          ; app
          ])
    in

    (* Multiplication and division, left associative *)
    let mul_div =
      let op =
        choice
          [ symbol "*" *> return (fun l r -> Binop (Mul, l, r))
          ; symbol "/" *> return (fun l r -> Binop (Div, l, r))
          ]
      in
      chainl1 unary op
    in

    (* Addition and subtraction, left associative *)
    let add_sub =
      let op =
        choice
          [ symbol "+" *> return (fun l r -> Binop (Add, l, r))
          ; symbol "-" *> return (fun l r -> Binop (Sub, l, r))
          ]
      in
      chainl1 mul_div op
    in

    (* Comparison operators have the lowest precedence among infix operators *)
    let cmp =
      let op =
        choice
          [ symbol "=" *> return (fun l r -> Cmp (Eq, l, r))
          ; symbol "<>" *> return (fun l r -> Cmp (Neq, l, r))
          ; symbol "<=" *> return (fun l r -> Cmp (Le, l, r))
          ; symbol ">=" *> return (fun l r -> Cmp (Ge, l, r))
          ; symbol "<" *> return (fun l r -> Cmp (Lt, l, r))
          ; symbol ">" *> return (fun l r -> Cmp (Gt, l, r))
          ]
      in
      chainl1 add_sub op
    in

    (* Desugar multi argument functions to nested [Abs] *)
    let make_fun params body =
      List.fold_right (fun x acc -> Abs (x, acc)) params body
    in

    (* [fun x y -> e] sugar for nested abstractions *)
    let fun_expr =
      symbol "fun"
      *> many1 ident
      >>= fun params ->
      symbol "->"
      *> expr
      >>| fun body -> make_fun params body
    in

    (* [if e1 then e2 else e3] expression, lowest precedence *)
    let if_expr =
      symbol "if"
      *> expr
      >>= fun cond ->
      symbol "then"
      *> expr
      >>= fun then_ ->
      symbol "else"
      *> expr
      >>| fun else_ -> If (cond, then_, else_)
    in

    (* [let] and [let rec] with optional curried arguments *)
    let let_expr =
      symbol "let"
      *> token ident_raw (* We read the next word, it could be 'rec' or a name *)
      >>= fun next_token ->
      let is_rec = next_token = "rec" in
      (* If "rec" was read, then the function name comes next (parse using ident) *)
      (* If "rec" wasn't read, then it's already a name (check that it's not a keyword) *)
      (if is_rec then ident else (
         if List.mem next_token keywords then fail "Keyword used as variable name"
         else return next_token
      ))
      >>= fun name ->
      many ident
      >>= fun params ->
      symbol "="
      *> expr
      >>= fun rhs ->
      symbol "in"
      *> expr
      >>| fun body ->
      let rhs' = make_fun params rhs in
      if is_rec then Let_rec (name, rhs', body) else Let (name, rhs', body)
    in

    (* Statement like forms have the lowest precedence, so we try them first *)
    choice [ let_expr; if_expr; fun_expr; cmp ])
;;

(* Keep [dispatch] around for compatibility, but both entries now parse full expressions *)
let parse_lam =
  let apps _ = expr in
  let single _ = expr in
  { apps; single }
;;

let parse str =
  match Angstrom.parse_string (spaces *> expr <* spaces) ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
