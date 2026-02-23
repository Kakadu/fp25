(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let spaces1 = take_while1 is_space *> return ()

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords = [ "if"; "then"; "else"; "let"; "rec"; "in"; "fix" ]

(* Парсер идентификаторов (имён переменных) *)
let identifier =
  lift2
    (fun first rest -> String.make 1 first ^ rest) (* Склеиваем первый символ и остаток *)
    (satisfy is_lower)
    (take_while is_ident_char)
  >>= fun s ->
  if List.mem s keywords
  then fail "keyword"
  else return s (* Проверка на ключевое слово *)
;;

let integer =
  lift2
    (fun sign digits ->
      let n = int_of_string digits in
      if sign then -n else n) (* Применяем знак, если есть минус *)
    (option false (char '-' *> return true))
    (take_while1 is_digit)
;;

(* Парсер ключевых слов с границей, после слова не идёт символ идентификатора*)
let keyword s =
  spaces *> string s *> peek_char
  >>= function
  | Some c when is_ident_char c -> fail "keyword followed by identifier char"
  | _ -> spaces
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

(* Комбинатор для лево-ассоциативных операторов *)
(* Пример: 1 + 2 + 3 парсится как (1 + 2) + 3 *)
let chainl1 p op =
  p
  >>= fun x ->
  many (lift2 (fun f y -> f, y) op p)
  >>| fun rest ->
  List.fold_left
    (fun acc (f, y) -> f acc y)
    x
    rest (* Применяем операторы слева направо *)
;;

let parser_impl =
  fix (fun expr ->
    let atom =
      spaces
      *> choice
           (* choice - пробует каждый парсер по порядку, возвращает первый успешный *)
           [ char '(' *> spaces *> expr <* spaces <* char ')'
           ; (integer >>| fun n -> Ast.Const n)
           ; (keyword "if" *> expr
              >>= fun cond ->
              keyword "then" *> expr
              >>= fun t1 -> keyword "else" *> expr >>| fun t2 -> Ast.If (cond, t1, t2))
             (* Let rec: рекурсивное связывание функции *)
           ; (keyword "let" *> keyword "rec" *> identifier
              >>= fun fname ->
              many (spaces1 *> identifier) (* Парсим параметры функции *)
              >>= fun params ->
              keyword "=" *> expr
              >>= fun body ->
              keyword "in" *> expr
              >>| fun in_expr ->
              match params with
              | [] -> Ast.Let (fname, body, in_expr) (* Без параметров - обычный let *)
              | param :: rest ->
                (* С параметрами - создаём вложенные лямбды: fun x y -> body становится fun x -> (fun y -> body) *)
                let full_body = List.fold_right (fun p b -> Ast.Abs (p, b)) rest body in
                Ast.LetRec (fname, param, full_body, in_expr))
           ; (keyword "let" *> identifier
              >>= fun name ->
              many (spaces1 *> identifier)
              >>= fun params ->
              keyword "=" *> expr
              >>= fun e1 ->
              keyword "in" *> expr
              >>| fun e2 ->
              (* Если есть параметры, превращаем в лямбду: let f x y = body → let f = fun x -> fun y -> body *)
              let full_body = List.fold_right (fun p b -> Ast.Abs (p, b)) params e1 in
              Ast.Let (name, full_body, e2))
           ; (keyword "fix" *> expr >>| fun e -> Ast.Fix e)
           ; (identifier >>| fun v -> Ast.Var v)
           ]
      <* spaces
    in
    (* Парсеры операторов *)
    (* Умножение и деление (высокий приоритет) *)
    let mult_op =
      spaces
      *> choice
           [ string "*" *> return (fun l r -> Ast.BinOp (Ast.Mul, l, r))
           ; string "/" *> return (fun l r -> Ast.BinOp (Ast.Div, l, r))
           ]
      <* spaces
    in
    (* Сложение и вычитание (средний приоритет) *)
    let add_op =
      spaces
      *> choice
           [ string "+" *> return (fun l r -> Ast.BinOp (Ast.Add, l, r))
           ; string "-" *> return (fun l r -> Ast.BinOp (Ast.Sub, l, r))
           ]
      <* spaces
    in
    (* Операции сравнения (низкий приоритет) *)
    let cmp_op =
      spaces
      *> choice
           [ string "<=" *> return (fun l r -> Ast.BinOp (Ast.Le, l, r))
           ; string ">=" *> return (fun l r -> Ast.BinOp (Ast.Ge, l, r))
           ; string "=" *> return (fun l r -> Ast.BinOp (Ast.Eq, l, r))
           ; string "<" *> return (fun l r -> Ast.BinOp (Ast.Lt, l, r))
           ; string ">" *> return (fun l r -> Ast.BinOp (Ast.Gt, l, r))
           ]
      <* spaces
    in
    (* Применение функции (f x y парсится как (f x) y) *)
    let app =
      many1 atom (* Парсим одну или более атомарных выражений *)
      >>| function
      | [] -> failwith "empty app"
      | [ x ] -> x
      | x :: xs -> List.fold_left (fun l r -> Ast.App (l, r)) x xs
    in
    let mult = chainl1 app mult_op in
    let add = chainl1 mult add_op in
    let cmp = chainl1 add cmp_op in
    choice
      [ ((string "fun" <|> string "λ" <|> string "\\")
         *> spaces
         *> many1 (spaces *> identifier <* spaces)
         >>= fun vars ->
         (string "->" <|> char '.' *> return "") *> cmp
         >>| fun body -> List.fold_right (fun v b -> Ast.Abs (v, b)) vars body)
      ; cmp
      ])
;;

(* Основная функция парсинга строки *)
let parse str =
  match Angstrom.parse_string parser_impl ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x (* Успех - возвращаем AST *)
  | Error er -> Result.Error (`Parsing_error er)
;;
