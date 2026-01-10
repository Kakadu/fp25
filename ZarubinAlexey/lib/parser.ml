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
let symbol (s : string) : string t = lexeme (string s)

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

(** парсер выражений с приоритетами*)
let rec expr () : Ast.name Ast.t t =
  (* сначала конструкции с ключевыми словами let/if/fun, а потом бинарные операции*)
  let_expr () <|> if_expr () <|> fun_expr () <|> cmp ()

(** парсер let / let rec*)
and let_expr () : Ast.name Ast.t t =
  let* _ = symbol "let" in
  let* is_rec = symbol "rec" *> return true <|> return false in
  (* let rec*)
  let* name = ident in
  if is_rec
  then
    let* param = ident in
    let* _ = symbol "=" in
    let* body = expr () in
    let* _ = symbol "in" in
    let* in_e = expr () in
    return (Ast.Let_rec (name, param, body, in_e))
  else
    (* let *)
    let* _ = symbol "=" in
    let* e1 = expr () in
    let* _ = symbol "in" in
    let* e2 = expr () in
    return (Ast.Let (name, e1, e2))

(* if_expr *)
and if_expr () : Ast.name Ast.t t =
  let* _ = symbol "if" in
  let* c = expr () in
  let* _ = symbol "then" in
  let* t_branch = expr () in
  let* _ = symbol "else" in
  let* e_branch = expr () in
  return (Ast.If (c, t_branch, e_branch))

(* парсер функции с сахаром fun x y - body *)
and fun_expr () : Ast.name Ast.t t =
  let* _ = symbol "fun" in
  (* many1 p парсим p 1 или больше раз и возвращаем список рез*)
  let* params = many1 ident in
  let* _ = symbol "->" in
  let* body = expr () in
  (* делаем каррирование для функции *)
  let lam = List.fold_right (fun p acc -> Ast.Abs (p, acc)) params body in
  return lam

(* =, <, > *)
and cmp () : Ast.name Ast.t t =
  let* left = add_sub () in
  let parse_op =
    symbol "=" *> return Ast.Eq
    <|> symbol "<" *> return Ast.Lt
    <|> symbol ">" *> return Ast.Gt
  in
  (let* op = parse_op in
   let* right = add_sub () in
   return (Ast.Binop (op, left, right)))
  <|> return left

(* +, - *)
and add_sub () : Ast.name Ast.t t =
  let op_add = symbol "+" *> return (fun e1 e2 -> Ast.Binop (Ast.Add, e1, e2)) in
  let op_sub = symbol "-" *> return (fun e1 e2 -> Ast.Binop (Ast.Sub, e1, e2)) in
  chainl1 (mul_div ()) (op_add <|> op_sub)

(* *, / *)
and mul_div () : Ast.name Ast.t t =
  let op_mul = symbol "*" *> return (fun e1 e2 -> Ast.Binop (Ast.Mul, e1, e2)) in
  let op_div = symbol "/" *> return (fun e1 e2 -> Ast.Binop (Ast.Div, e1, e2)) in
  chainl1 (app ()) (op_mul <|> op_div)

and app () : Ast.name Ast.t t =
  let* first = atom () in
  let* rest = many (atom ()) in
  return (List.fold_left (fun acc arg -> Ast.App (acc, arg)) first rest)

(* atom : числа, перем, выражение и fix expr *)
and atom () : Ast.name Ast.t t =
  (* пробуем по очереди, пока не сработает что то *)
  choice
    [ (* скобки *)
      parens (expr ())
    ; (* fix e *)
      (symbol "fix" *> atom () >>= fun e -> return (Ast.Fix e))
    ; (* число *)
      integer
    ; (* переменная *)
      (ident >>= fun x -> return (Ast.Var x))
    ]
;;

(** связка с типом*)
type dispatch =
  { apps : dispatch -> Ast.name Ast.t Angstrom.t
  ; single : dispatch -> Ast.name Ast.t Angstrom.t
  }

let parse_lam : dispatch =
  let single (_pack : dispatch) : Ast.name Ast.t Angstrom.t = expr () in
  let apps (_pack : dispatch) : Ast.name Ast.t Angstrom.t = expr () in
  { single; apps }
;;

let parse (str : string) : (Ast.name Ast.t, [> error ]) result =
  match
    Angstrom.parse_string
      ~consume:Angstrom.Consume.All
      (spaces *> parse_lam.apps parse_lam <* end_of_input)
      str
  with
  | Result.Ok x -> Result.Ok x
  | Result.Error msg -> Result.Error (`Parsing_error msg)
;;
