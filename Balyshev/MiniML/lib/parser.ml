open Angstrom
open Ast

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_char_valid_for_name = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
  | _ -> false
;;

let parens p = char '(' *> ws *> p <* ws <* char ')'

let is_keyword = function
  | "fun"
  | "in"
  | "let"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "match"
  | "with"
  | "type"
  | "and"
  | "of"
  | "_" -> true
  | _ -> false
;;

let name_fabric regexp error_message =
  let* chs = ws *> take_while1 is_char_valid_for_name in
  if is_keyword chs
  then fail "unexpected keyword"
  else if Str.string_match (Str.regexp regexp) chs 0
  then return chs
  else fail error_message
;;

let var_name =
  let regexp = "^[a-z_][a-zA-Z0-9_]*$" in
  let message = "not a variable name" in
  name_fabric regexp message
;;

let constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_]*$" in
  let message = "not a constructor name" in
  name_fabric regexp message
;;

let type_name =
  let regexp = "^[a-z_][a-zA-Z0-9_]*$" in
  let message = "not a type name" in
  name_fabric regexp message
;;

let type_param_name =
  let regexp = "^'[a-zA-Z][a-zA-Z0-9_]*$" in
  let message = "not a type param name" in
  name_fabric regexp message
;;

let constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_]*$" in
  let message = "not a constructor name" in
  name_fabric regexp message
;;

type dispatch_patt =
  { patt_basic : dispatch_patt -> pattern t
  ; patt_cons : dispatch_patt -> pattern t
  ; patt_tuple : dispatch_patt -> pattern t
  ; patt : dispatch_patt -> pattern t
  }

let pnil = PConstruct ("Nil", None)
let enil = EConstruct ("Nil", None)
let pcons hd tl = PConstruct ("Cons", Some (PTuple (hd, tl, [])))
let econs hd tl = EConstruct ("Cons", Some (ETuple (hd, tl, [])))

let patt_basic d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> parens (d.patt d)
    <|> char '_' *> return PAny
    <|> (var_name >>| fun v -> PVar v)
    <|> string "[]" *> return pnil
    <|> (char '['
         *> ws
         *>
         let* first = d.patt d in
         (let* rest = many (ws *> char ';' *> d.patt d) in
          return (pcons first (List.fold_right pcons rest pnil)))
         <* ws
         <* char ']')
    <|> let* name = ws *> constructor_name in
        (let* patt = ws *> d.patt_basic d in
         return (PConstruct (name, Some patt)))
        <|> return (PConstruct (name, None)))
;;

let patt_cons d =
  ws
  *> fix (fun _self ->
    return (fun head tail -> pcons head tail)
    <*> d.patt_basic d
    <*> ws *> string "::" *> ws *> d.patt_cons d
    <|> d.patt_basic d)
;;

let patt_tuple d =
  ws
  *> fix (fun _self ->
    return (fun a b xs -> PTuple (a, b, xs))
    <*> (d.patt_cons d <* ws)
    <*> (char ',' *> d.patt_cons d <* ws)
    <*> many (char ',' *> d.patt_cons d <* ws))
;;

let pattern : pattern t =
  let patt = fun d -> d.patt_tuple d <|> d.patt_cons d <|> d.patt_basic d in
  patt { patt; patt_basic; patt_cons; patt_tuple }
;;

type dispatch_expr =
  { expr_basic : dispatch_expr -> expression t
  ; expr_tuple : dispatch_expr -> expression t
  ; expr : dispatch_expr -> expression t
  ; add_sub : dispatch_expr -> expression t
  ; mul_div : dispatch_expr -> expression t
  ; cons : dispatch_expr -> expression t
  ; cmp : dispatch_expr -> expression t
  }

let left_chain op init_t item_t =
  return (fun init items -> List.fold_left (fun a b -> EBinop (op, a, b)) init items)
  <*> ws *> init_t
  <*> many1 (ws *> item_t)
;;

let right_chain op init_t item_t =
  return (fun init items -> List.fold_right (fun a b -> EBinop (op, a, b)) items init)
  <*> ws *> init_t
  <*> many1 (ws *> item_t)
;;

let cmp d =
  ws
  *> fix (fun _self ->
    parens (d.expr d)
    <|> left_chain Ne (d.cons d) (string "<>" *> ws *> d.cons d)
    <|> left_chain Le (d.cons d) (string "<=" *> ws *> d.cons d)
    <|> left_chain Ge (d.cons d) (string ">=" *> ws *> d.cons d)
    <|> left_chain Lt (d.cons d) (char '<' *> ws *> d.cons d)
    <|> left_chain Gt (d.cons d) (char '>' *> ws *> d.cons d)
    <|> left_chain Eq (d.cons d) (char '=' *> ws *> d.cons d)
    <|> d.cons d)
;;

let cons d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> parens (d.expr d)
    <|> right_chain Cons (d.add_sub d) (string "::" *> ws *> d.add_sub d)
    <|> d.add_sub d)
;;

let add_sub d =
  ws
  *> fix (fun _self ->
    parens (d.expr d)
    <|> left_chain Add (d.mul_div d) (char '+' *> ws *> d.mul_div d)
    <|> left_chain Sub (d.mul_div d) (char '-' *> ws *> d.mul_div d)
    <|> d.mul_div d)
;;

let mul_div d =
  ws
  *> fix (fun _self ->
    parens (d.expr d)
    <|> left_chain Mul (d.expr_basic d) (char '*' *> ws *> d.mul_div d)
    <|> left_chain Div (d.expr_basic d) (char '/' *> ws *> d.mul_div d)
    <|> d.expr_basic d)
;;

let expr_basic d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> parens (d.expr d)
    <|> string "()" *> return (EConst CUnit)
    <|> (var_name >>| fun v -> EVar v)
    <|> (take_while1 is_digit >>| fun chs -> EConst (CInt (int_of_string chs)))
    <|> string "[]" *> return enil
    <|> (char '['
         *> ws
         *>
         let* first = d.expr d in
         (let* rest = many (ws *> char ';' *> d.expr d) in
          return (econs first (List.fold_right econs rest enil)))
         <* ws
         <* char ']')
    <|> let* name = ws *> constructor_name in
        (let* patt = ws *> d.expr_basic d in
         return (EConstruct (name, Some patt)))
        <|> return (EConstruct (name, None)))
;;

let expr_tuple d =
  ws
  *> fix (fun _self ->
    return (fun a b xs -> ETuple (a, b, xs))
    <*> (d.cons d <* ws)
    <*> (char ',' *> d.cons d <* ws)
    <*> many (char ',' *> d.cons d <* ws))
;;

let expression : expression t =
  let expr =
    fun d ->
    choice [ d.expr d; d.expr_tuple d; d.cmp d; d.cons d; d.add_sub d; d.mul_div d ]
  in
  expr { expr = expr_tuple; cons; cmp; expr_tuple; expr_basic; add_sub; mul_div }
;;

let parse_expression text = parse_string ~consume:All (expression <* end_of_input) text
