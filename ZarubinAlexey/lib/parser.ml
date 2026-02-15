(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom

let ( let* ) = ( >>= )

(** заворачиваем текстовое сообщение*)
type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

(** проверяем является ли символ пробельным*)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(** пропускаем сколько угодно пробельных символов*)
let spaces : unit t = skip_while is_space

let lexeme (p : 'a t) : 'a t = p <* spaces

(** парсим конкретную строку потом пробелы*)
let symbol (s : string) : string t = spaces *> string s <* spaces

let parens (p : 'a t) : 'a t = symbol "(" *> p <* symbol ")"

(** символы которые можно использовать в начале идентификатора*)
let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

(**символ которые можно использовать внутри идентификатора*)

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

(** парсим идентификатор, первая буква (берем 1 символ, если он удовлетворяет предикату)
    , потом берем строку из 0 и более символ подходящих под предикат, потом собираем строку (первая буква + хвост) *)

let ident : string t =
  let* first = satisfy is_ident_start in
  let* rest = take_while is_ident_char in
  let name = String.make 1 first ^ rest in
  lexeme (return name)
;;

(** парсим число *)
let integer : Ast.name Ast.t t =
  let* sign = option 1 (char '-' *> return (-1)) in
  let* digits =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  let n = int_of_string digits in
  let value = sign * n in
  lexeme (return (Ast.Int value))
;;

(** бинарные операции (комбинатор),
    парсер операндов,
    парсер бaнарной операции*)

let chainl1
  (p : Ast.name Ast.t t)
  (op : (Ast.name Ast.t -> Ast.name Ast.t -> Ast.name Ast.t) t)
  : Ast.name Ast.t t
  =
  let rec go acc =
    (let* f = op in
     let* x = p in
     let acc' = f acc x in
     go acc')
    <|> return acc
  in
  let* first = p in
  go first
;;

(** Парсер выражений с приоритетами.
    Важно: используем [Angstrom.fix], чтобы рекурсивный парсер [expr]
    можно было безопасно использовать внутри [atom] (для скобок).
    Это убирает stack overflow, который возникает при взаимных вызовах expr()/atom()
    на этапе построения парсеров. *)
let expr : Ast.name Ast.t t =
  fix (fun expr ->
      (** let-выражение:
          - let x = e1 in e2
          - let rec f x = body in in_e
            Здесь let rec поддерживаем в форме "функция с одним аргументом",
            как в большинстве эталонных решений. *)
      let let_expr : Ast.name Ast.t t =
        let* _ = symbol "let" in
        (* опциональное "rec" *)
        let* is_rec = (symbol "rec" *> return true) <|> return false in
        (* имя после let: либо x, либо f *)
        let* name = ident in
        if is_rec
        then (
          (* let rec f x = body in in_e *)
          let* param = ident in
          let* _ = symbol "=" in
          let* body = expr in
          let* _ = symbol "in" in
          let* in_e = expr in
          return (Ast.Let_rec (name, param, body, in_e))
        )
        else (
          (* let x = e1 in e2 *)
          let* _ = symbol "=" in
          let* e1 = expr in
          let* _ = symbol "in" in
          let* e2 = expr in
          return (Ast.Let (name, e1, e2))
        )
      in

      (** if-выражение:
          if cond then e1 else e2 *)
      let if_expr : Ast.name Ast.t t =
        let* _ = symbol "if" in
        let* c = expr in
        let* _ = symbol "then" in
        let* t_branch = expr in
        let* _ = symbol "else" in
        let* e_branch = expr in
        return (Ast.If (c, t_branch, e_branch))
      in

      (** fun-выражение:
          fun x y z -> body
          Список параметров сворачиваем вправо:
          fun x y -> body  ==  Abs(x, Abs(y, body)) *)
      let fun_expr : Ast.name Ast.t t =
        let* _ = symbol "fun" in
        let* params = many1 ident in
        let* _ = symbol "->" in
        let* body = expr in
        let lam = List.fold_right (fun p acc -> Ast.Abs (p, acc)) params body in
        return lam
      in

      (** atom — самый низкий уровень грамматики.
              Делается через Angstrom.fix, чтобы рекурсивная ссылка (fix atom)
              не вызывала немедленного вызова atom при построении парсера. *)
      let atom : Ast.name Ast.t t =
        fix (fun atom ->
            choice
              [ (** (expr) *)
                parens expr
                ; (** fix e *)
                (symbol "fix" *> atom >>= fun e -> return (Ast.Fix e))
                ; (** число *)
                integer
                ; (** переменная *)
                (ident >>= fun x -> return (Ast.Var x))
              ])
      in

      (** app — применение функций: f x y -> ((f x) y) *)
      let app : Ast.name Ast.t t =
        let* first = atom in
        let* rest = many atom in
        return (List.fold_left (fun acc arg -> Ast.App (acc, arg)) first rest)
      in

      (** unary — префиксный минус:
          -x  ==>  0 - x
          Это нужно для (n - 1) внутри скобок и для -1. *)
      let unary : Ast.name Ast.t t =
        (symbol "-" *> (app >>= fun e -> return (Ast.Binop (Ast.Sub, Ast.Int 0, e))))
        <|> app
      in

      (** mul_div — уровень * и / *)
      let mul_div : Ast.name Ast.t t =
        let op_mul = symbol "*" *> return (fun e1 e2 -> Ast.Binop (Ast.Mul, e1, e2)) in
        let op_div = symbol "/" *> return (fun e1 e2 -> Ast.Binop (Ast.Div, e1, e2)) in
        chainl1 unary (op_mul <|> op_div)
      in

      (** add_sub — уровень + и - *)
      let add_sub : Ast.name Ast.t t =
        let op_add = symbol "+" *> return (fun e1 e2 -> Ast.Binop (Ast.Add, e1, e2)) in
        let op_sub = symbol "-" *> return (fun e1 e2 -> Ast.Binop (Ast.Sub, e1, e2)) in
        chainl1 mul_div (op_add <|> op_sub)
      in

      (** cmp — сравнения (=, <, >). Если оператора нет — возвращаем left. *)
      let cmp : Ast.name Ast.t t =
        let* left = add_sub in
        let parse_op =
          (symbol "=" *> return Ast.Eq)
          <|> (symbol "<" *> return Ast.Lt)
          <|> (symbol ">" *> return Ast.Gt)
        in
        (let* op = parse_op in
         let* right = add_sub in
         return (Ast.Binop (op, left, right)))
        <|> return left
      in
      (** Верхний уровень выражения:
              сначала ключевые слова (let/if/fun),
              иначе — обычное выражение с операторами. *)
      let_expr <|> if_expr <|> fun_expr <|> cmp
    )
;;

let parse (str : string) : (Ast.name Ast.t, [> error ]) result =
  match
    Angstrom.parse_string
      ~consume:Angstrom.Consume.All
      (spaces *> expr <* spaces <* end_of_input)
      str
  with
  | Result.Ok x -> Result.Ok x
  | Result.Error msg -> Result.Error (`Parsing_error msg)
;;
