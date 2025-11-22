(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let token p = p <* spaces
let parens p = token (char '(') *> p <* token (char ')')

(** Парсер целых чисел *)
let number =
  let positive = token
    (take_while1 (function
      | '0' .. '9' -> true
      | _ -> false))
  in
  token
    (option '+' (char '-')
     >>= fun sign ->
     positive
     >>| fun num ->
     match sign with
     | '-' -> Ast.Num (-(int_of_string num))
     | _ -> Ast.Num (int_of_string num)
    )
;;

(** Пареср названий переменных *)
let varname =
  (* В названии переменной допускаются только символы из латинского алфавит и _ *)
  let is_var_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  token (take_while1 is_var_char)
  >>= fun s ->
  match s with
  | "if" | "then" | "else" | "fun" | "let" | "rec" | "letrec" | "in" | "fix" | "print" ->
    fail ("reserved keyword: " ^ s)
  | _ -> return s
;;

(** Unary operations *)
let unop = token (string "++" *> return Ast.Inc <|> string "--" *> return Ast.Dec)


(** Бинарные операции *)
let mult_div_op = token (char '*' *> return Ast.Mult <|> char '/' *> return Ast.Div)
let add_div_op = token (char '+' *> return Ast.Plus <|> char '-' *> return Ast.Minus)

let expr =
  fix (fun expr ->
    let atom =
      choice [
        number;
        varname >>| (fun v -> Ast.Var v);
        parens expr;
      ]
    in
    let simple_expr =
      choice
        [ number
        ; (varname >>| fun v -> Ast.Var v)
        ; parens expr
        ; (token (string "fix") *> expr >>| fun e -> Ast.Fix e)
        ; (token (string "print") *> expr >>| fun e -> Ast.Print e)
        ; (unop >>= fun op -> atom <|> parens expr >>| fun e -> Ast.Unop (op, e))
        ]
    in
    (** Парсер для применения функций (левоассоциативный) *)
    let app_expr =
      many1 simple_expr
      >>= function
      | [] -> fail "empty application"
      | x :: xs -> return (List.fold_left (fun f arg -> Ast.App (f, arg)) x xs)
    in
    let mult_expr =
      app_expr >>= fun first ->
      let rec parse_rest left =
        choice [
          (mult_div_op >>= fun op -> app_expr >>= fun right -> 
            parse_rest (Ast.Binop (op, left, right)));
          return left
        ]
      in
      parse_rest first
    in

    let add_expr =
      mult_expr >>= fun first ->
      let rec parse_rest left =
        choice [
          (add_div_op >>= fun op -> mult_expr >>= fun right -> 
            parse_rest (Ast.Binop (op, left, right)));
          return left
        ]
      in
      parse_rest first
    in
    (* Парсер для if выражений *)
    let if_expr =
      token (string "if") *> expr
      >>= fun cond ->
      token (string "then") *> expr
      >>= fun then_branch ->
      token (string "else") *> expr
      >>= fun else_branch -> return (Ast.If (cond, then_branch, else_branch))
    in
    (** Парсер для функций (с поддержкой многопараметрического сахара) *)
    let fun_expr =
      token (string "fun") *> many1 varname
      >>= fun args ->
      token (string "->") *> expr
      >>= fun body ->
      (* Десугаризация: fun x y -> e => fun x -> fun y -> e *)
      let multi_fun = List.fold_right (fun arg body -> Ast.Fun (arg, body)) args body in
      return multi_fun
    in
    (** Парсер для let выражений *)
    let let_expr =
      token (string "let") *> (token (string "rec") *> return true <|> return false)
      >>= fun is_rec ->
      varname
      >>= fun name ->
      token (char '=') *> expr
      >>= fun value ->
      token (string "in") *> expr
      >>= fun body ->
      if is_rec
      then return (Ast.Letrec (name, value, body))
      else return (Ast.Let (name, value, body))
    in
    choice [ if_expr; fun_expr; let_expr; add_expr ])
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse str =
  match Angstrom.parse_string ~consume:Consume.All (expr <* spaces) str with
  | Result.Ok x -> Result.Ok x
  | Error msg -> Result.Error (`Parsing_error msg)
;;
