open Ast
open Utils

type parse_error =
  | PExpectedInt
  | PExpectedFloat
  | PExpectedId
  | PExpectedRightOperand
  | PSyntaxError

let pp_parse_error fmt = function
  | PExpectedInt -> Format.fprintf fmt "expected integer"
  | PExpectedFloat -> Format.fprintf fmt "expected float"
  | PExpectedId -> Format.fprintf fmt "expected identifier"
  | PExpectedRightOperand -> Format.fprintf fmt "expected right operand"
  | PSyntaxError -> Format.fprintf fmt "syntax error"
;;

type input = char list [@@deriving show]

type 'a parse_result =
  | Failed of parse_error
  | Parsed of 'a * input
[@@deriving show]

type 'a parser = input -> 'a parse_result

let keywords = [ "let"; "in"; "if"; "then"; "else"; "fun"; "rec"; "true"; "false" ]
let return x str = Parsed (x, str)

let ( >>= ) parser f str =
  match parser str with
  | Failed s -> Failed s
  | Parsed (x, str') -> f x str'
;;

let ( <|> ) p1 p2 str =
  match p1 str with
  | Failed _ -> p2 str
  | Parsed _ as ok -> ok
;;

let ( *> ) p1 p2 = p1 >>= fun _ -> p2
let ( <* ) p1 p2 = p1 >>= fun h -> p2 >>= fun _ -> return h
let ( let* ) = ( >>= )
let fail err _ = Failed err

let choice = function
  | [] -> fail PSyntaxError
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

let rec many : 'a parser -> 'a list parser =
  fun p s ->
  match p s with
  | Failed _ -> return [] s
  | Parsed (x, rest) -> (many p >>= fun tl -> return (x :: tl)) rest
;;

let many1 p = p >>= fun x -> many p >>= fun xs -> return (x :: xs)

let satisfy cond = function
  | c :: str when cond c -> return c str
  | _ -> Failed PSyntaxError
;;

let p_char c = satisfy (Char.equal c)

let p_string str =
  String.fold_left
    (fun acc value ->
      acc
      >>= fun h ->
      let* c = p_char value in
      return (h ^ String.make 1 c))
    (return "")
    str
;;

let p_digit =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy is_digit
;;

let p_letter =
  let is_letter = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  satisfy is_letter
;;

let p_int =
  let* digits = many1 p_digit in
  return (EConst (IConst (int_of_string (charlst_to_str digits)))) <|> fail PExpectedInt
;;

let p_float =
  let* l = many1 p_digit in
  let* _ = p_char '.' in
  let* r = many p_digit in
  return (EConst (FConst (float_of_string (charlst_to_str (l @ [ '.' ] @ r)))))
  <|> fail PExpectedFloat
;;

let is_keyword word = List.exists (fun kwd -> word = kwd) keywords

let p_id =
  let* first = p_letter in
  let* rest = many (p_letter <|> p_digit) in
  let id = charlst_to_str (first :: rest) in
  if is_keyword id then fail PExpectedId else return id
;;

let p_ws = many (p_char ' ' <|> p_char '\t' <|> p_char '\n')
let token p = p <* p_ws
let p_add = token (p_char '+') *> return Add
let p_sub = token (p_char '-') *> return Sub
let p_mul = token (p_char '*') *> return Mul
let p_div = token (p_char '/') *> return Div
let p_fadd = token (p_string "+.") *> return AddF
let p_fsub = token (p_string "-.") *> return SubF
let p_fmul = token (p_string "*.") *> return MulF
let p_fdiv = token (p_string "/.") *> return DivF
let p_eq = token (p_char '=') *> return Eq
let p_neq = token (p_string "<>") *> return Neq
let p_leq = token (p_string "<=") *> return Leq
let p_geq = token (p_string ">=") *> return Geq
let p_lt = token (p_string "<") *> return Lt
let p_gt = token (p_string ">") *> return Gt
let p_and = token (p_string "&&") *> return And
let p_or = token (p_string "||") *> return Or
let parens p = token (p_char '(') *> token p <* token (p_char ')')

let p_word word =
  token
    (many p_letter
     >>= fun lst -> if charlst_to_str lst = word then return word else fail PSyntaxError)
;;

let p_bool =
  p_word "true" *> return (EConst (BConst true))
  <|> p_word "false" *> return (EConst (BConst false))
;;

let p_const = token (p_float <|> p_int <|> p_bool)

let binop_chain binop_lst next_parser left =
  let rec loop left input =
    match token (choice binop_lst) input with
    | Parsed (op, rest1) ->
      (match token next_parser rest1 with
       | Parsed (right, rest2) -> loop (EBinOp (op, left, right)) rest2
       | Failed e -> Failed e)
    | Failed _ -> Parsed (left, input)
  in
  loop left
;;

let rec_label input =
  match p_word "rec" input with
  | Parsed (_, input') -> Parsed (Recursive, input')
  | Failed _ -> Parsed (Nonrecursive, input)
;;

let p_expr =
  let rec expr input = (token binop_expr_bool1) input
  and binop_expr_bool1 input =
    (let* left = token binop_expr_bool2 in
     token (binop_chain [ p_or ] binop_expr_bool2 left))
      input
  and binop_expr_bool2 input =
    (let* left = token binop_expr_bool3 in
     token (binop_chain [ p_and ] binop_expr_bool3 left))
      input
  and binop_expr_bool3 input =
    (let* left = token binop_expr in
     token (binop_chain [ p_eq; p_neq; p_leq; p_geq; p_lt; p_gt ] binop_expr left))
      input
  and binop_expr input =
    (let* left = token term in
     token (binop_chain [ p_fadd; p_fsub; p_add; p_sub ] term left))
      input
  and term input =
    (let* left = token factor in
     token (binop_chain [ p_fmul; p_fdiv; p_mul; p_div ] factor left))
      input
  and factor input = func_apply input
  and func_apply input =
    (let* left = atomic in
     let* right = many atomic in
     match right with
     | [] -> return left
     | _ -> return (List.fold_left (fun acc arg -> EApp (acc, arg)) left right))
      input
  and atomic input =
    (token (choice [ parens expr; var; p_const; func; let_expr; if_expr ])) input
  and var input =
    (let* id = token p_id in
     return (EVar id))
      input
  and let_expr input =
    (let* _ = token (p_word "let") in
     let* label = rec_label in
     let* left = token let_bind in
     let* _ = token (p_word "in") in
     let* right = token expr in
     return (ELet (label, left, right)))
      input
  and let_bind input =
    (let* left = token var <* token (p_char '=') in
     let* right = token expr in
     return (Bind (left, right)))
      input
  and if_expr input =
    (let* _ = token (p_word "if") in
     let* cond = token expr in
     let* _ = token (p_word "then") in
     let* if_body = token expr in
     let* _ = token (p_word "else") in
     let* else_body = token expr in
     return (EIf (cond, if_body, else_body)))
      input
  and func input =
    (let* _ = token (p_word "fun") in
     let* args = many1 var in
     let* _ = token (p_string "->") in
     let* right = token expr in
     return (List.fold_left (fun acc arg -> EFun (arg, acc)) right (List.rev args)))
      input
  in
  expr
;;

let p_toplevel_let =
  let* _ = token (p_word "let") in
  let* label = rec_label in
  let* name = token p_id in
  let* _ = token (p_char '=') in
  let* body = token p_expr in
  return (TopLet (label, Bind (EVar name, body)))
;;

let p_toplevel_expr =
  let* e = p_expr in
  return (TopExpr e)
;;

let p_toplevel = token (p_toplevel_expr <|> p_toplevel_let)

let p_final input =
  let res = p_toplevel input in
  match res with
  | Parsed (_, lst) when lst <> [] -> Failed PSyntaxError
  | _ -> res
;;

let parser str = p_final (str_to_charlst str)
