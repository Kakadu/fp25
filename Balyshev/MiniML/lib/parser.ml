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
  | "true"
  | "false"
  | "_" -> true
  | _ -> false
;;

let name_fabric regexp error_message =
  let* chs = ws *> take_while1 is_char_valid_for_name in
  if is_keyword chs
  then fail (Printf.sprintf "unexpected keyword: %s" chs)
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
  ; expr_long : dispatch_expr -> expression t
  ; expr_tuple : dispatch_expr -> expression t
  ; expr : dispatch_expr -> expression t
  ; add_sub : dispatch_expr -> expression t
  ; mul_div : dispatch_expr -> expression t
  ; cons : dispatch_expr -> expression t
  ; cmp : dispatch_expr -> expression t
  ; expr_top : dispatch_expr -> expression t
  }

let left_chain op init_t item_t =
  return (fun init items -> List.fold_left (fun a b -> EBinop (op, a, b)) init items)
  <*> ws *> init_t
  <*> many1 (ws *> item_t)
;;

let right_chain f first_t item_t =
  let rec helper items acc =
    match items with
    | [] -> acc
    | x :: xs -> helper xs (f x acc)
  in
  let* first = ws *> first_t in
  let* rest = many1 (ws *> item_t) in
  match List.rev (first :: rest) with
  | x :: xs -> return (helper xs x)
  | _ -> fail "unreachable"
;;

let cmp d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> left_chain Ne (d.add_sub d) (string "<>" *> ws *> d.add_sub d)
    <|> left_chain Le (d.add_sub d) (string "<=" *> ws *> d.add_sub d)
    <|> left_chain Ge (d.add_sub d) (string ">=" *> ws *> d.add_sub d)
    <|> left_chain Lt (d.add_sub d) (char '<' *> ws *> d.add_sub d)
    <|> left_chain Gt (d.add_sub d) (char '>' *> ws *> d.add_sub d)
    <|> left_chain Eq (d.add_sub d) (char '=' *> ws *> d.add_sub d)
    <|> d.add_sub d)
;;

let cons d =
  ws
  *> fix (fun _self ->
    right_chain
      (fun hd tl -> EBinop (Cons, hd, tl))
      (d.add_sub d)
      (string "::" *> ws *> d.add_sub d)
    <|> d.add_sub d)
;;

let add_sub d =
  ws
  *> fix (fun _self ->
    left_chain Add (d.mul_div d) (char '+' *> ws *> d.mul_div d)
    <|> left_chain Sub (d.mul_div d) (char '-' *> ws *> d.mul_div d)
    <|> d.mul_div d)
;;

let mul_div d =
  ws
  *> fix (fun _self ->
    left_chain Mul (d.expr_long d) (char '*' *> ws *> d.expr_long d)
    <|> left_chain Div (d.expr_long d) (char '/' *> ws *> d.expr_long d)
    <|> d.expr_long d)
;;

let expr_long d =
  ws
  *> fix (fun _self ->
    many (ws *> d.expr_basic d)
    >>= function
    | [] -> fail "not an expr_long or expr_basic"
    | x :: [] -> return x
    | f :: xs -> List.fold_left (fun f x -> EApp (f, x)) f xs |> return)
;;

let expr_basic d =
  ws
  *> fix (fun _self ->
    parens (d.expr d)
    <|> string "()" *> return (EConstant CUnit)
    <|> string "true" *> return (EConstant (CBool true))
    <|> string "false" *> return (EConstant (CBool false))
    <|> string "()" *> return (EConstant CUnit)
    <|> (var_name >>| fun v -> EVar v)
    <|> (take_while1 is_digit >>| fun chs -> EConstant (CInt (int_of_string chs)))
    <|> string "[]" *> return enil
    <|> (char '['
         *> ws
         *> (return (fun frst rest -> econs frst (List.fold_right econs rest enil))
             <*> d.expr d
             <*> many (ws *> char ';' *> ws *> d.expr d))
         <* ws
         <* char ']')
    <|> (return (fun name arg -> EConstruct (name, arg))
         <*> ws *> constructor_name
         <*> (ws *> d.expr_basic d >>| Option.some <|> return None)))
;;

let expr_tuple d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> (return (fun a b xs -> ETuple (a, b, xs))
         <*> (d.expr_top d <|> d.cmp d)
         <*> (char ',' *> (d.expr_top d <|> d.cmp d) <* ws)
         <*> many (char ',' *> (d.expr_top d <|> d.cmp d) <* ws)))
;;

let expr_top d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> (return (fun cond expr_then expr_else -> EIf (cond, expr_then, expr_else))
         <*> ws *> string "if" *> ws *> d.expr d
         <*> ws *> string "then" *> ws *> d.expr d
         <*> ws *> string "else" *> ws *> d.expr d)
    <|> (return (fun patts expr -> List.fold_right (fun p e -> EFun (p, e)) patts expr)
         <*> string "fun" *> many (ws *> pattern)
         <*> ws *> string "->" *> ws *> d.expr d)
    <|> string "let"
        *> ws
        *> (return (fun rec_flag patt expr body -> ELet (rec_flag, patt, expr, body))
            <*> (string "rec" *> return Recursive <|> return NonRecursive)
            <*> ws *> pattern
            <*> (ws *> char '=' *> ws *> d.expr d <* ws <* string "in")
            <*> ws *> d.expr d))
;;

let expression : expression t =
  let expr d =
    ws
    *> fix (fun _self ->
      d.expr_top d
      <|> d.expr_tuple d
      <|> d.cmp d
      (* <|> d.cons d *)
      <|> d.add_sub d
      <|> d.mul_div d
      <|> d.expr_long d
      <|> d.expr_basic d)
  in
  expr { expr; expr_long; cons; cmp; expr_tuple; expr_basic; add_sub; mul_div; expr_top }
;;

let parse_expression text = parse_string ~consume:All (expression <* end_of_input) text
let parse_pattern text = parse_string ~consume:All (pattern <* end_of_input) text
