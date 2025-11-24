(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let split_arg = satisfy is_space >>= fun c -> spaces >>| fun () -> c
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
let add_sub_op = token (char '+' *> return Ast.Plus <|> char '-' *> return Ast.Minus)

let multi_fun args = List.fold_right (fun arg body -> Ast.Fun (arg, body)) args

let expr =
  fix (fun expr ->
    let unop_expr =
      unop >>= fun op ->
        choice [number; (varname >>| (fun v -> Ast.Var v))] >>| fun var ->
          Ast.Unop (op, var)
    in
    (** Парсер для функций (с поддержкой многопараметрического сахара) *)
    let fun_expr =
      token (string "fun") *> many1 varname
      >>= fun args ->
      token (string "->") *> expr
      >>= fun body ->
      (* Десугаризация: fun x y -> e => fun x -> fun y -> e *)
      return @@ multi_fun args body
    in
  

  let app_expr =
    choice [fun_expr ; (varname >>| (fun v -> Ast.Var v))] >>= fun name ->
    many1(choice [ number; parens fun_expr; (varname >>| (fun v -> Ast.Var v)) ]) >>| fun args ->
      List.fold_left (fun f arg -> Ast.App (f, arg)) name args
  in

  let unary_expr =
      choice [
          number
        ; app_expr
        ; (varname >>| (fun v -> Ast.Var v))
        ; unop_expr
        ; parens expr
        ; fun_expr
      ]
    in

  let mult_expr =
    unary_expr >>= fun first ->
    many
          (mult_div_op >>= fun op -> unary_expr >>| fun right -> op, right)
        >>| List.fold_left (fun left (op, right) -> Ast.Binop (op, left, right)) first
  in

  let add_expr =
    mult_expr >>= fun first ->
    many (add_sub_op >>= fun op -> mult_expr >>| fun right -> op, right)
        >>| List.fold_left (fun left (op, right) -> Ast.Binop (op, left, right)) first
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
    
    (** Парсер для let выражений *)
    let let_expr =
      token (string "let") *> (token (string "rec") *> return true <|> return false)
      >>= fun is_rec ->
      varname
      >>= fun name ->
      many varname
      >>= fun args ->
      token (char '=') *> expr
      >>= fun value ->
      let body = match args with
      | [] -> value
      | _ -> (multi_fun args value)
      in
      option None (token (string "in") *> expr >>| fun cont -> Some cont)
      >>= fun res -> 
      if(is_rec)
      then return @@ Ast.Letrec (name, body, res)
      else return @@ Ast.Let (name, body, res)
    in
    choice [ if_expr; fun_expr; let_expr; add_expr])
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse str =
  match Angstrom.parse_string ~consume:Consume.All (spaces *> expr <* spaces) str with
  | Result.Ok x -> Result.Ok x
  | Error msg -> Result.Error (`Parsing_error msg)
;;
