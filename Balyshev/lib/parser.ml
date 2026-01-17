open Angstrom
open Parsetree

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
  | "and"
  | "true"
  | "false"
  | "_"
  | "type"
  | "of" -> true
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
let brackets p = char '[' *> ws *> p <* ws <* char ']'

let parse_constant =
  string "()" *> return CUnit
  <|> string "true" *> return (CBool true)
  <|> string "false" *> return (CBool false)
  <|> (take_while1 is_digit >>| fun chs -> CInt (int_of_string chs))
;;

let patt_basic d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> parens (d.patt d)
    <|> char '_' *> return PAny
    <|> (parse_constant >>| fun x -> PConstant x)
    <|> (var_name >>| fun v -> PVar v)
    <|> string "[]" *> return pnil
    <|> brackets
          (let* first = d.patt d in
           let* rest = many (ws *> char ';' *> d.patt d) in
           return (pcons first (List.fold_right pcons rest pnil)))
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
  let patt d = d.patt_tuple d <|> d.patt_cons d <|> d.patt_basic d in
  patt { patt; patt_basic; patt_cons; patt_tuple }
;;

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

let expr_list_brackets expr =
  brackets
    (let* first = expr in
     let* rest = many (ws *> char ';' *> expr) in
     return (econs first (List.fold_right econs rest enil)))
;;

let expr_list_cons expr =
  fix (fun self ->
    let* head = ws *> expr in
    (let* () = ws <* string "::" in
     let* tail = ws *> self in
     return (econs head tail))
    <|> return head)
;;

let rec_flag = ws *> (string "rec" *> return Recursive <|> return NonRecursive)

let expr_binding expr =
  let* patterns = many (ws *> pattern) in
  let* expr = ws *> char '=' *> ws *> expr in
  match patterns with
  | [] -> fail "not an expr_binding"
  | p :: ps ->
    let rec desugar acc = function
      | [] -> acc
      | p :: ps -> desugar (EFun (p, acc)) ps
    in
    return (p, desugar expr (List.rev ps))
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

let expr_tuple expr =
  return (fun a b xs -> ETuple (a, b, xs))
  <*> expr_complex expr
  <*> ws *> char ',' *> expr_complex expr
  <*> many (ws *> char ',' *> expr_complex expr)
;;

let expr_atom =
  fail ""
  <|> (parse_constant >>| fun x -> EConstant x)
  <|> char '[' *> ws *> char ']' *> return enil
  <|> (var_name >>| fun v -> EVar v)
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

let binop_left_alter expr op_sep_ls =
  let make_parser (op, sep) =
    return (fun b a -> EBinop (op, a, b)) <*> ws *> string sep *> ws *> expr
  in
  let parsers = List.map make_parser op_sep_ls in
  return (fun init fs -> List.fold_left (fun acc f -> f acc) init fs)
  <*> ws *> expr
  <*> many1 (choice parsers)
;;

let fold_alter ~cases ~init =
  Base.List.fold cases ~init ~f:(fun acc expr -> expr acc <|> acc)
;;

let expr_binop expr =
  let cmp expr =
    binop_left_alter expr [ Ne, "<>"; Le, "<="; Ge, ">="; Eq, "=="; Lt, "<"; Gt, ">" ]
  in
  let add_sub expr = binop_left_alter expr [ Add, "+"; Sub, "-" ] in
  let mul_div expr = binop_left_alter expr [ Mul, "*"; Div, "/" ] in
  fold_alter ~init:expr ~cases:[ mul_div; add_sub; cmp ]
;;

let expression =
  ws
  *> fix (fun self ->
    fold_alter
      ~init:(expr_atom <|> parens self <|> expr_list_brackets self)
      ~cases:
        [ constructor; expr_long; expr_list_cons; expr_binop; expr_tuple; expr_complex ])
;;

let parse_expression text = parse_string ~consume:All (expression <* end_of_input) text

let type_param_tuple =
  ws *> char '(' *> ws *> sep_by (ws *> char ',') (ws *> type_param_name)
  >>= function
  | frst :: scnd :: rest -> return (frst :: scnd :: rest) <* ws *> char ')'
  | _ -> fail "tuple of param names expected"
;;

let core_type_arrow core_type =
  fix (fun self ->
    let* operand = ws *> core_type in
    ws *> string "->" *> ws *> self
    >>| (fun operand2 -> Pty_arrow (operand, operand2))
    <|> return operand)
;;

let core_type_tuple core_type =
  let* first = ws *> core_type in
  many (ws *> char '*' *> ws *> core_type)
  >>= function
  | [] -> return first
  | second :: rest -> return (Pty_tuple (first, second, rest))
;;

let core_type =
  ws
  *> fix (fun self ->
    let prims =
      fail ""
      <|> (type_name >>| fun v -> Pty_constr (v, []))
      <|> (type_param_name >>| fun v -> Pty_var v)
    in
    let prims =
      (let* arg = prims in
       let* name = ws *> type_name in
       return (Pty_constr (name, [ arg ])))
      <|> prims
    in
    let self = parens self <|> prims in
    let self = core_type_tuple self <|> self in
    let self = core_type_arrow self <|> self in
    (let* ct = char '(' *> ws *> self in
     let* cts = many (ws *> char ',' *> self) in
     let* name = ws *> char ')' *> type_name in
     return (Pty_constr (name, ct :: cts)))
    <|> self)
;;

let type_params =
  ws
  *> (type_param_name
      >>| (fun param -> [ param ])
      <|> parens (type_param_name >>| fun param -> [ param ])
      <|> type_param_tuple
      <|> return [])
;;

let type_kind_variants =
  let parse_variant =
    let* name = ws *> char '|' *> ws *> constructor_name in
    (let* ct = ws *> string "of" *> ws *> core_type in
     return (name, Some ct))
    <|> return (name, None)
  in
  many parse_variant
  >>= function
  | var :: vars -> return (Pty_variants (var, vars))
  | _ -> fail "is not variants"
;;

let type_kind_abstract = ws *> core_type >>| fun x -> Pty_abstract (Some x)
let type_kind = ws *> (type_kind_variants <|> type_kind_abstract)

let type_body =
  let* params = ws *> type_params in
  let* name = ws *> type_name in
  let* kind = ws *> char '=' *> ws *> type_kind in
  return { pty_params = params; pty_name = name; pty_kind = kind }
;;

let type_declaration =
  return (fun td tds -> Pstr_type (td, tds))
  <*> ws *> string "type" *> ws *> type_body
  <*> many (ws *> string "and" *> type_body)
;;

let value_binding =
  return (fun rec_flag vb vbs -> Pstr_value (rec_flag, (vb, vbs)))
  <*> ws *> string "let" *> rec_flag
  <*> ws *> expr_binding expression
  <*> many (ws *> string "and" *> expr_binding expression)
;;

let structure =
  return (fun item items -> item, items)
  <*> (type_declaration <|> value_binding)
  <*> many (type_declaration <|> value_binding)
;;

let parse_structure text = parse_string ~consume:All (structure <* end_of_input) text

(* testing stuff *)
let parse_pattern text = parse_string ~consume:All (pattern <* end_of_input) text
let parse_core_type text = parse_string ~consume:All (core_type <* end_of_input) text
