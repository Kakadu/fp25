open Angstrom
open Ast
open Base

let spaces = take_while1 Char.is_whitespace >>| fun _ -> ()
let skip_opt_spaces = skip_while Char.is_whitespace

let is_keyword = function
  | "let" | "rec" | "if" | "then" | "else" | "fun" | "fix" | "in" -> true
  | _ -> false
;;

(* ================= вспомогательные парсеры ================= *)

let parse_identifier =
  let* fst_char =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let id = fst_char ^ rest in
  let is_key = is_keyword id in
  if is_key
  then fail ("keyword cannot be identifier: " ^ id)
  else return (Id (fst_char ^ rest))
;;

let parse_constant =
  take_while1 Char.is_digit >>| fun s -> Expr_const (Const_int (Int.of_string s))
;;

(* ================= бинарные операции ================= *)

let parse_additive_operator =
  skip_opt_spaces *> take 1
  >>= function
  | "+" -> return Plus
  | "-" -> return Sub
  | _ -> fail "not additive operator"
;;

let parse_multiplicative_operator =
  skip_opt_spaces *> take 1
  >>= function
  | "*" -> return Mul
  | "/" -> return Div
  | _ -> fail "not multiplicative operator"
;;

(* ================= главный парсер через fix ================= *)

let parse_expression =
  fix (fun expr ->
    (* атомы *)
    let parse_identifier_expression = parse_identifier >>| fun id -> Expr_var id in
    let parse_parens =
      char '(' *> skip_opt_spaces *> expr <* skip_opt_spaces <* char ')'
    in
    let parse_if_expression =
      string "if" *> spaces *> expr
      >>= fun cond ->
      spaces *> string "then" *> spaces *> expr
      >>= fun t ->
      spaces *> string "else" *> spaces *> expr >>| fun e -> Expr_conditional (cond, t, e)
    in
    let parse_function_expression =
      string "fun" *> many1 (spaces *> parse_identifier)
      >>= fun args ->
      spaces *> string "->" *> spaces *> expr
      >>| fun body ->
      List.fold_right args ~init:body ~f:(fun arg acc -> Expr_fun (arg, acc))
    in
    let parse_let_in_expression =
      string "let" *> spaces *> parse_identifier
      >>= fun id ->
      skip_opt_spaces *> char '=' *> skip_opt_spaces *> expr
      >>= fun value ->
      spaces *> string "in" *> spaces *> expr >>| fun body -> Expr_let_in (id, value, body)
    in
    let parse_let_rec_in_expression =
      string "let" *> spaces *> string "rec" *> spaces *> parse_identifier
      >>= fun id ->
      skip_opt_spaces *> char '=' *> skip_opt_spaces *> expr
      >>= fun value ->
      spaces *> string "in" *> spaces *> expr
      >>| fun body -> Expr_let_rec_in (id, value, body)
    in
    let parse_fix_expression = string "fix" *> spaces *> expr >>| fun e -> Expr_fix e in
    let parse_atomic_expression =
      (* порядок важен: специальные ключевые слова и парсер скобок сначала *)
      parse_parens
      <|> parse_if_expression
      <|> parse_function_expression
      <|> parse_let_rec_in_expression
      <|> parse_let_in_expression
      <|> parse_fix_expression
      <|> parse_constant
      <|> parse_identifier_expression
    in
    (* application: обязать прогресс — между функцией и аргументом требуется по крайней мере один пробел *)
    let parse_function_application =
      let* func = parse_atomic_expression in
      let* args = many (spaces *> parse_atomic_expression) in
      match args with
      | [] -> return func
      | _ -> return (Expr_ap (func, args))
    in
    (* multiplicative *)
    let parse_multiplicative_expression =
      let* left = parse_function_application in
      let* rest =
        many
          (let* op = parse_multiplicative_operator in
           skip_opt_spaces
           *>
           let* right = parse_function_application in
           return (op, right))
      in
      return
        (List.fold_left rest ~init:left ~f:(fun acc (op, r) ->
           Expr_binary_op (op, acc, r)))
    in
    (* additive *)
    let parse_additive_expression =
      let* left = parse_multiplicative_expression in
      let* rest =
        many
          (let* op = parse_additive_operator in
           skip_opt_spaces
           *>
           let* right = parse_multiplicative_expression in
           return (op, right))
      in
      return
        (List.fold_left rest ~init:left ~f:(fun acc (op, r) ->
           Expr_binary_op (op, acc, r)))
    in
    parse_additive_expression)
;;

(* ================= парсеры верхнего уровня ================= *)

let parse_toplevel_let =
  string "let" *> spaces *> parse_identifier
  >>= fun id ->
  skip_opt_spaces *> char '=' *> skip_opt_spaces *> parse_expression
  >>= fun expr ->
  skip_opt_spaces *> string ";;" *> skip_opt_spaces >>| fun () -> Top_let (id, expr)
;;

let parse_toplevel_let_rec =
  string "let" *> spaces *> string "rec" *> spaces *> parse_identifier
  >>= fun id ->
  skip_opt_spaces *> char '=' *> skip_opt_spaces *> parse_expression
  >>= fun expr ->
  skip_opt_spaces *> string ";;" *> skip_opt_spaces >>| fun () -> Top_let_rec (id, expr)
;;

let parse_structure = many (parse_toplevel_let_rec <|> parse_toplevel_let)
let parse = parse_string ~consume:All parse_structure
