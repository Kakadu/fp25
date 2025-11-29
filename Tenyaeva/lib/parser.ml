(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

(* ==================== auxiliary ==================== *)

let ws = skip_while Char.is_whitespace
let ws1 = skip Char.is_whitespace *> ws
let token str = ws *> string str
let skip_round_par parse = token "(" *> parse <* token ")"

let is_keyword = function
  | "let"
  | "rec"
  | "and"
  | "in"
  | "if"
  | "then"
  | "else"
  | "match"
  | "with"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "type"
  | "_" -> true
  | _ -> false
;;

let chain_left parse p_function =
  let rec go acc = lift2 (fun f x -> f acc x) p_function parse >>= go <|> return acc in
  parse >>= go
;;

let rec chain_right parse p_function =
  let* left = parse in
  (let* f = p_function in
   let* right = chain_right parse p_function in
   return (f left right))
  <|> return left
;;

(* ==================== constant ==================== *)

let parse_int =
  take_while1 Char.is_digit >>| fun int_value -> Const_int (Int.of_string int_value)
;;

let parse_unit = token "()" *> return Const_unit
let parse_const = ws *> choice [ parse_int; parse_unit ]

(* ==================== ident ==================== *)

let parse_ident =
  ws
  *>
  let* fst_char =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* rest_str =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let id = fst_char ^ rest_str in
  if is_keyword id then fail (Printf.sprintf "Impossible name: %S." id) else return id
;;

(* --------------------- type ---------------------- *)

let parse_type_var =
  token "'"
  *>
  let* name = parse_ident in
  return (Type_var ("'" ^ name))
;;

let parse_base_type =
  choice [ token "unit" *> return Type_unit; token "int" *> return Type_int ]
;;

let parse_type_option p_type =
  let* t = p_type <* token "option" in
  return (Type_option t)
;;

let rec parse_type_arrow p_type =
  let* l_type = p_type in
  (let* r_type = token "->" *> parse_type_arrow p_type in
   return (Type_arrow (l_type, r_type)))
  <|> return l_type
;;

let parse_type =
  fix (fun self ->
    let atom = parse_base_type <|> parse_type_var <|> skip_round_par self in
    let opt = parse_type_option atom <|> atom in
    let arr = parse_type_arrow opt <|> opt in
    arr)
;;

(* -------------------- pattern -------------------- *)

let parse_pat_any = token "_" *> return Pat_any
let parse_pat_const = parse_const >>| fun c -> Pat_constant c
let parse_pat_var = parse_ident >>| fun i -> Pat_var i

let parse_pat_option parse_pat =
  token "Some" *> parse_pat
  >>| (fun e -> Some e)
  <|> token "None" *> return None
  >>| fun e -> Pat_option e
;;

let parse_pat_constraint parse_pat =
  let* pat = token "(" *> parse_pat in
  let* constr = token ":" *> parse_type <* token ")" in
  return (Pat_constraint (constr, pat))
;;

let parse_pattern =
  fix (fun self ->
    choice
      [ parse_pat_const
      ; parse_pat_var
      ; parse_pat_any
      ; parse_pat_option self
      ; parse_pat_constraint self
      ])
;;

(* -------------------- operation -------------------- *)

let cmp =
  choice
    [ token "=" *> return Eq
    ; token "<>" *> return Neq
    ; token "<=" *> return Lte
    ; token ">=" *> return Gte
    ; token "<" *> return Lt
    ; token ">" *> return Gt
    ]
;;

let add_sub = choice [ token "+" *> return Add; token "-" *> return Sub ]
let mult_div = choice [ token "/" *> return Div; token "*" *> return Mult ]

let parse_binop parse_expr parse_op =
  chain_left parse_expr (parse_op >>| fun opr exp1 exp2 -> Expr_binop (opr, exp1, exp2))
;;

let parse_expr_binop parse_expr =
  let parse_expr = parse_binop parse_expr mult_div in
  let parse_expr = parse_binop parse_expr add_sub in
  parse_binop parse_expr cmp
;;

let parse_unop = choice [ token "-" *> return Negative; token "+" *> return Positive ]

let parse_expr_unop parse_expr =
  let* op = parse_unop in
  let* e = parse_expr in
  return (Expr_unop (op, e))
;;

(* -------------------- expression -------------------- *)

let parse_rec_flag = token "rec" *> return Recursive <|> return NonRecursive
let parse_expr_const = parse_const >>| fun c -> Expr_const c
let parse_expr_ident = parse_ident >>| fun i -> Expr_ident i

let parse_expr_option parse_expr =
  token "Some" *> parse_expr
  >>| (fun op -> Some op)
  <|> token "None" *> return None
  >>| fun e -> Expr_option e
;;

let parse_expr_constraint parse_expr =
  let* expr = token "(" *> parse_expr in
  let* constr = token ":" *> parse_type <* token ")" in
  return (Expr_constraint (constr, expr))
;;

let parse_expr_fun parse_expr =
  let* pat = token "fun" *> parse_pattern in
  let* params = many parse_pattern in
  let* body_expr = token "->" *> parse_expr in
  let expr =
    match params with
    | [] -> body_expr
    | _ -> List.fold_right ~f:(fun par acc -> Expr_fun (par, acc)) params ~init:body_expr
  in
  return (Expr_fun (pat, expr))
;;

let parse_expr_apply parse_expr =
  chain_left parse_expr (return (fun e1 e2 -> Expr_apply (e1, e2)))
;;

let parse_expr_if parse_expr =
  let* cond = token "if" *> parse_expr in
  let* expr_then = token "then" *> parse_expr in
  let* expr_else = token "else" *> parse_expr >>| (fun e -> Some e) <|> return None in
  return (Expr_if (cond, expr_then, expr_else))
;;

let parse_value_binding parse_expr =
  let* pattern = parse_pattern in
  let* pat_list = many parse_pattern in
  let+ expression = token "=" *> parse_expr in
  { vb_pat = pattern
  ; vb_expr =
      (match pat_list with
       | [] -> expression
       | _ -> List.fold_right ~f:(fun f p -> Expr_fun (f, p)) pat_list ~init:expression)
  }
;;

let parse_let parse_expr =
  let* rec_flag = token "let" *> parse_rec_flag in
  let* vb = parse_value_binding parse_expr in
  let* value_bindings = many (token "and" *> parse_value_binding parse_expr) in
  let+ expr = token "in" *> parse_expr in
  Expr_let (rec_flag, vb, value_bindings, expr)
;;

let parse_case parse_expr =
  let* pat = token "|" *> parse_pattern in
  let* expr = token "->" *> parse_expr in
  return { case_pat = pat; case_expr = expr }
;;

let parse_expr_function parse_expr =
  let* case = token "function" *> parse_case parse_expr in
  let* casel = many (parse_case parse_expr) in
  return (Expr_function (case, casel))
;;

let parse_expr_match parse_expr =
  let* expr = token "match" *> parse_expr <* token "with" in
  let* case = parse_case parse_expr in
  let* casel = many (parse_case parse_expr) in
  return (Expr_match (expr, case, casel))
;;

let parse_expression =
  fix (fun self ->
    let atom =
      choice
        [ skip_round_par self
        ; parse_expr_const
        ; parse_expr_ident
        ; parse_expr_constraint self
        ; parse_expr_option self
        ]
    in
    let expr_if = parse_expr_if self <|> atom in
    let expr_unop = parse_expr_unop expr_if <|> expr_if in
    let expr_binop = parse_expr_binop expr_unop <|> expr_unop in
    let expr_match = parse_expr_match expr_binop <|> expr_binop in
    let expr_functon = parse_expr_function expr_match <|> expr_match in
    let expr_app = parse_expr_apply expr_functon <|> expr_functon in
    let expr_fun = parse_expr_fun expr_app <|> expr_app in
    let expr_let = parse_let expr_fun <|> expr_fun in
    expr_let)
;;

(* ==================== structure ==================== *)

let parse_structure_value parse_exp =
  token "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* vb = parse_value_binding parse_exp in
  let+ value_bindings = many (token "and" *> parse_value_binding parse_exp) in
  Str_value (rec_flag, vb, value_bindings)
;;

let parse_structure =
  let str_value = parse_structure_value parse_expression in
  let str_eval = str_value <|> (parse_expression >>| fun ex -> Str_eval ex) in
  let semicolons = many (token ";;") in
  sep_by semicolons str_eval <* semicolons <* ws
;;

(* ==================== execute ==================== *)

let parse = parse_string ~consume:All parse_structure
