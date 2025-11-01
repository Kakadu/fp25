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

let pnil = PConstruct ("[]", None)
let enil = EConstruct ("[]", None)
let pcons hd tl = PConstruct ("::", Some (PTuple (hd, tl, [])))
let econs hd tl = EConstruct ("::", Some (ETuple (hd, tl, [])))

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

let parse_pattern text = parse_string ~consume:All (pattern <* end_of_input) text

let expr_ite expr =
  return (fun cond expr_then expr_else -> EIf (cond, expr_then, expr_else))
  <*> ws *> string "if" *> ws *> expr
  <*> ws *> string "then" *> ws *> expr
  <*> ws *> string "else" *> ws *> expr
;;

let expr_fun expr =
  return (fun patts expr -> List.fold_right (fun p e -> EFun (p, e)) patts expr)
  <*> ws *> string "fun" *> many1 (ws *> pattern)
  <*> ws *> string "->" *> ws *> expr
;;

let match_first_case expr =
  return (fun p e -> p, e)
  <*> ws *> (char '|' <|> return '|') *> ws *> pattern
  <*> ws *> string "->" *> ws *> expr
;;

let match_case expr =
  return (fun p e -> p, e)
  <*> ws *> char '|' *> ws *> pattern
  <*> ws *> string "->" *> ws *> expr
;;

let expr_match expr =
  return (fun subject case cases -> EMatch (subject, (case, cases)))
  <*> ws *> string "match" *> ws *> expr
  <*> ws *> string "with" *> ws *> match_first_case expr
  <*> many (match_case expr)
;;

let rec_flag = ws *> (string "rec" *> return Recursive <|> return NonRecursive)

let expr_binding expr =
  let* patt = ws *> pattern in
  let* expr = ws *> char '=' *> ws *> expr in
  return (patt, expr)
;;

let expr_let expr =
  return (fun rec_flag vb vbs body -> ELet (rec_flag, (vb, vbs), body))
  <*> ws *> string "let" *> rec_flag
  <*> ws *> expr_binding expr
  <*> many (ws *> string "and" *> expr_binding expr)
  <*> ws *> string "in" *> ws *> expr
;;

let expr_complex expr =
  fix (fun self ->
    fail ""
    <|> expr_match self
    <|> expr_ite self
    <|> expr_let self
    <|> expr_fun self
    <|> expr)
;;

(** parses [ fun x -> x, fun y -> y ] as [ fun x -> (x, fun y -> y) ] *)
(** parses [ let x = x in x, y ] as [ let x = x in (x, y) ] *)
let expr_tuple expr =
  return (fun a b xs -> ETuple (a, b, xs))
  <*> expr_complex expr
  <*> (char ',' *> expr_complex expr <* ws)
  <*> many (char ',' *> expr_complex expr <* ws)
;;

let expr_atom =
  fail ""
  <|> string "()" *> return (EConstant CUnit)
  <|> string "true" *> return (EConstant (CBool true))
  <|> string "false" *> return (EConstant (CBool false))
  <|> char '[' *> ws *> char ']' *> return enil
  <|> (take_while1 is_digit >>| fun chs -> EConstant (CInt (int_of_string chs)))
  <|> (var_name >>| fun v -> EVar v)
;;

let expr_list expr =
  char '['
  *> ws
  *> (return (fun frst rest -> econs frst (List.fold_right econs rest enil))
      <*> expr
      <*> many (ws *> char ';' *> ws *> expr))
  <* ws
  <* char ']'
;;

let constant_constructor = ws *> constructor_name >>| fun name -> EConstruct (name, None)

let constructor expr =
  return (fun name arg -> EConstruct (name, arg))
  <*> ws *> constructor_name
  <*> (ws *> (expr <|> constant_constructor) >>| Option.some <|> return None)
;;

let expr_long expr =
  ws *> many (ws *> expr)
  >>= function
  | [] -> fail "not an expr_long nor an expr_basic"
  | x :: [] -> return x
  | f :: xs -> List.fold_left (fun f x -> EApp (f, x)) f xs |> return
;;

let left_chain expr op sep =
  return (fun init items -> List.fold_left (fun a b -> EBinop (op, a, b)) init items)
  <*> ws *> expr
  <*> many1 (ws *> string sep *> ws *> expr)
;;

let cmp expr =
  fail ""
  <|> left_chain expr Ne "<>"
  <|> left_chain expr Ne "<="
  <|> left_chain expr Ge ">="
  <|> left_chain expr Eq "=="
  <|> left_chain expr Lt "<"
  <|> left_chain expr Gt ">"
;;

let add_sub expr = left_chain expr Add "+" <|> left_chain expr Sub "-"
let mul_div expr = left_chain expr Mul "*" <|> left_chain expr Div "/"

let fold_alter ~cases ~init =
  Base.List.fold cases ~init ~f:(fun acc expr -> expr acc <|> acc)
;;

let expr_binop expr = fold_alter ~init:expr ~cases:[ mul_div; add_sub; cmp ]

let expression =
  ws
  *> fix (fun self ->
    fold_alter
      ~init:(expr_atom <|> parens self <|> expr_list self)
      ~cases:[ expr_long; constructor; expr_binop; expr_tuple; expr_complex ])
;;

let parse_expression text = parse_string ~consume:All (expression <* end_of_input) text
