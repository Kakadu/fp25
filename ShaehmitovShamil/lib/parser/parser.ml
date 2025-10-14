open Ast
open Base
open Angstrom

let whitespace = take_while Char.is_whitespace
let keyword s = string s <* whitespace
let parens p = char '(' *> whitespace *> p <* whitespace <* char ')'
let token p = p <* whitespace

(**Constatnts*)
let parse_integer = take_while1 Char.is_digit >>| fun digits -> Int (Int.of_string digits)

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


let parse_binary_op parsed_bin_op =
  return (fun e1 e2 -> BinOp (parsed_bin_op, e1, e2))
;;

let parse_unary_op parsed_un_op =
  return (fun e -> match parsed_un_op with Neg -> BinOp (Sub, Int 0, e) | Not -> BinOp (Neq, e, Bool true))
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
  return (match param with
          | Var name -> Fun (name, body)
          | _ -> failwith "impossible: parse_name always returns Var")
;;

let parse_app atom =
  let* first = atom in
  let* rest = many atom in
  return (List.fold_left ~f:(fun acc arg -> App (acc, arg)) ~init:first rest)
;;






