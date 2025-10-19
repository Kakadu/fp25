open Ast
open Base
open Angstrom

let whitespace = take_while Char.is_whitespace
let keyword s = string s <* whitespace
let parens p = char '(' *> whitespace *> p <* whitespace <* char ')' <* whitespace
let token p = p <* whitespace

(**Constatnts*)
let parse_integer =
  token (take_while1 Char.is_digit) >>| fun digits -> Int (Int.of_string digits)
;;

let parse_boolean =
  choice
    [ (token (string "true") >>| fun _ -> Bool true)
    ; (token (string "false") >>| fun _ -> Bool false)
    ]
;;

let is_name_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords = [ "let"; "rec"; "in"; "if"; "then"; "else"; "fun"; "true"; "false" ]

let parse_name =
  let* first = satisfy is_name_start in
  let* rest = take_while is_name_char in
  let* _ = whitespace in
  let name = String.of_char first ^ rest in
  if List.mem keywords name ~equal:String.equal
  then fail ("keyword " ^ name ^ " cannot be an identifier")
  else return (Var name)
;;

(**Binary operators *)
let add_op = token (string "+") *> return Add

let sub_op = token (string "-") *> return Sub
let mul_op = token (string "*") *> return Mul
let div_op = token (string "/") *> return Div
let eq_op = token (string "=") *> return Eq
let neq_op = token (string "<>") *> return Neq
let lt_op = token (string "<") *> return Lt
let gt_op = token (string ">") *> return Gt
let leq_op = token (string "<=") *> return Le
let geq_op = token (string ">=") *> return Ge
let and_op = token (string "&&") *> return And
let or_op = token (string "||") *> return Or

(**Unary operators *)
let neg_op = token (string "-") *> return Neg

let not_op = token (string "not") *> return Not

(**Keywords*)

let kw_let = keyword "let"
let kw_rec = keyword "rec"
let kw_in = keyword "in"
let kw_if = keyword "if"
let kw_then = keyword "then"
let kw_else = keyword "else"
let kw_fun = keyword "fun"
let parse_binary_op parsed_bin_op e1 e2 = BinOp (parsed_bin_op, e1, e2)

let parse_unary_op parsed_un_op =
  return (fun e ->
    match parsed_un_op with
    | Neg -> UnOp (Neg, e)
    | Not -> UnOp (Not, e))
;;

let parse_if expr =
  let* _ = kw_if in
  let* cond = expr in
  let* _ = kw_then in
  let* then_branch = expr in
  let* _ = kw_else in
  let* else_branch = expr in
  return (If (cond, then_branch, else_branch))
;;

let parse_lambda expr =
  let* _ = kw_fun in
  let* param = parse_name in
  let* _ = token (string "->") in
  let* body = expr in
  return
    (match param with
     | Var name -> Fun (name, body)
     | _ -> failwith "impossible: parse_name always returns Var")
;;

let parse_app atom =
  let* first = atom in
  (* let _ =print_endline (show_expr first) in *)
  let* rest = many atom in
  (* let _ = *)
  (* print_endline ("Rest: [" ^ String.concat ~sep:"; " (List.map ~f:show_expr rest) ^ "]") in *)
  return (List.fold_left ~f:(fun acc arg -> App (acc, arg)) ~init:first rest)
;;

let parse_let expr =
  let* is_rec = kw_let *> option false (kw_rec *> return true) in
  let* name = parse_name in
  let* params = many parse_name in
  let* _ = token (char '=') in
  let* value = expr in
  let* _ = kw_in in
  let* body = expr in
  match name with
  | Var fname ->
    let value_with_lambdas =
      List.fold_right
        ~f:(fun param acc ->
          match param with
          | Var pname -> Fun (pname, acc)
          | _ -> failwith "impossible")
        ~init:value
        params
    in
    return
      (if is_rec
       then LetRec (fname, value_with_lambdas, body)
       else Let (fname, value_with_lambdas, body))
  | _ -> fail "expected variable name in let"
;;

let parse_operators base_parser =
  (* Unary operators *)
  let unary =
    fix (fun unary_rec ->
      choice
        [ (not_op *> unary_rec >>| fun e -> UnOp (Not, e))
        ; (neg_op *> unary_rec >>| fun e -> UnOp (Neg, e))
        ; token base_parser
        ])
  in
  (* helper for left accociativity *)
  let chainl1 p op =
    let rec loop acc = lift2 (fun f x -> f acc x) op p >>= loop <|> return acc in
    p >>= loop
  in
  (* Priority of binary operators *)
  let mul_div = chainl1 unary (choice [ mul_op; div_op ] >>| parse_binary_op) in
  let add_sub = chainl1 mul_div (choice [ add_op; sub_op ] >>| parse_binary_op) in
  let comparison =
    chainl1
      add_sub
      (choice [ eq_op; neq_op; leq_op; geq_op; lt_op; gt_op ] >>| parse_binary_op)
  in
  let logical_and = chainl1 comparison (and_op >>| parse_binary_op) in
  let logical_or = chainl1 logical_and (or_op >>| parse_binary_op) in
  logical_or
;;

let parse_expr =
  fix (fun parse_expr ->
    let atom = choice [ parens parse_expr; parse_integer; parse_boolean; parse_name ] in
    let application = parse_app atom in
    let expr_with_ops = parse_operators application in
    choice
      [ parse_if parse_expr
      ; parse_lambda parse_expr
      ; parse_let parse_expr
      ; expr_with_ops
      ])
;;

let parse_program = whitespace *> parse_expr <* whitespace <* end_of_input

let parse s =
  match Angstrom.parse_string ~consume:All parse_program s with
  | Ok result -> Ok result
  | Error msg -> Error msg
;;
