(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

(* ==================== auxiliary ==================== *)

let ws = skip_while Char.is_whitespace
let ws1 = skip Char.is_whitespace *> ws
let token str = ws *> string str

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let keyword str =
  token str *> peek_char
  >>= function
  | Some c when is_ident_char c ->
    fail (Printf.sprintf "There is no separator after %S." str)
  | _ -> return str
;;

let skip_round_par parse = token "(" *> parse <* token ")"

let chain_left parse p_function =
  let rec go acc = lift2 (fun f x -> f acc x) p_function parse >>= go <|> return acc in
  parse >>= go
;;

(* ==================== constant ==================== *)

let parse_int =
  let* n = take_while1 Char.is_digit in
  peek_char
  >>= function
  | Some c when is_ident_char c -> fail "There is no separator after integer"
  | _ -> return (Const_int (Int.of_string n))
;;

let parse_unit = keyword "()" *> return Const_unit

let parse_bool =
  choice
    [ keyword "true" *> return (Const_bool true)
    ; keyword "false" *> return (Const_bool false)
    ]
;;

let parse_const = ws *> choice [ parse_int; parse_unit; parse_bool ]

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
  let* rest_str = take_while is_ident_char in
  let id = fst_char ^ rest_str in
  if is_keyword id then fail (Printf.sprintf "Impossible name: %S." id) else return id
;;

(* --------------------- type ---------------------- *)

let parse_type_var =
  token "'"
  *>
  let* name = parse_ident in
  return (Type_var name)
;;

let parse_base_type =
  choice
    [ keyword "unit" *> return Type_unit
    ; keyword "int" *> return Type_int
    ; keyword "bool" *> return Type_bool
    ]
;;

let parse_type_option p_type =
  let* t = p_type <* ws1 <* keyword "option" in
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

let parse_pat_any = keyword "_" *> return Pat_any
let parse_pat_const = parse_const >>| fun c -> Pat_constant c
let parse_pat_var = parse_ident >>| fun i -> Pat_var i

let parse_pat_option parse_pat =
  keyword "Some" *> parse_pat
  >>| (fun e -> Some e)
  <|> keyword "None" *> return None
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
      ; skip_round_par self
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

let parse_unop =
  choice
    [ token "-" *> return Negative
    ; token "+" *> return Positive
    ; keyword "not" *> return Not
    ]
;;

let parse_expr_unop parse_expr =
  let* op = parse_unop in
  let* e = parse_expr in
  return (Expr_unop (op, e))
;;

(* -------------------- expression -------------------- *)

let parse_rec_flag = keyword "rec" *> return Recursive <|> return NonRecursive
let parse_expr_const = parse_const >>| fun c -> Expr_const c
let parse_expr_ident = parse_ident >>| fun i -> Expr_ident i

let parse_expr_option parse_expr =
  keyword "Some" *> parse_expr
  >>| (fun op -> Some op)
  <|> keyword "None" *> return None
  >>| fun e -> Expr_option e
;;

let parse_expr_constraint parse_expr =
  let* expr = token "(" *> parse_expr in
  let* constr = token ":" *> parse_type <* token ")" in
  return (Expr_constraint (constr, expr))
;;

let parse_expr_fun parse_expr =
  let* pat = keyword "fun" *> parse_pattern in
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
  chain_left parse_expr (ws1 *> return (fun e1 e2 -> Expr_apply (e1, e2)))
;;

let parse_expr_if parse_expr =
  let* cond = keyword "if" *> parse_expr in
  let* expr_then = ws1 *> keyword "then" *> parse_expr in
  let* expr_else =
    ws1 *> keyword "else" *> parse_expr >>| (fun e -> Some e) <|> return None
  in
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
  let* rec_flag = keyword "let" *> parse_rec_flag in
  let* vb = parse_value_binding parse_expr in
  let* value_bindings = many (ws1 *> keyword "and" *> parse_value_binding parse_expr) in
  let+ expr = ws1 *> keyword "in" *> parse_expr in
  Expr_let (rec_flag, vb, value_bindings, expr)
;;

let parse_case parse_expr =
  let* pat = token "|" *> parse_pattern in
  let* expr = token "->" *> parse_expr in
  return { case_pat = pat; case_expr = expr }
;;

let parse_expr_function parse_expr =
  let* case = keyword "function" *> parse_case parse_expr in
  let* casel = many (ws1 *> parse_case parse_expr) in
  return (Expr_function (case, casel))
;;

let parse_expr_match parse_expr =
  let* expr = keyword "match" *> parse_expr <* ws1 <* keyword "with" in
  let* case = parse_case parse_expr in
  let* casel = many (ws1 *> parse_case parse_expr) in
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
    let expr_app = parse_expr_apply expr_if <|> expr_if in
    let expr_unop = parse_expr_unop expr_app <|> expr_app in
    let expr_binop = parse_expr_binop expr_unop <|> expr_unop in
    let expr_match = parse_expr_match self <|> expr_binop in
    let expr_functon = parse_expr_function self <|> expr_match in
    let expr_let = parse_let self <|> expr_functon in
    let expr_fun = parse_expr_fun self <|> expr_let in
    expr_fun)
;;

(* ==================== structure ==================== *)

let parse_structure_value =
  keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* vb = parse_value_binding parse_expression in
  let+ value_bindings = many (keyword "and" *> parse_value_binding parse_expression) in
  Str_value (rec_flag, vb, value_bindings)
;;

let parse_structure_eval = parse_expression >>| fun ex -> Str_eval ex
let parse_structure_item = parse_structure_eval <|> parse_structure_value

let parse_structure =
  let psemicolon = many (token ";;") in
  sep_by psemicolon parse_structure_item <* psemicolon <* ws
;;

(* ==================== execute ==================== *)

let parse = parse_string ~consume:All parse_structure
