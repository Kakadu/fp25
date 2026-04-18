[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | UnexpectedEnd
  | UnexpectedRest of string
  | SyntaxError of string

type 'a result =
  | Parsed of 'a * char list
  | ParseError of error

let return res chars = Parsed (res, chars)
let fail error _ = ParseError error

let fix =
  let rec fix f s = f (fix f) s in
  fix
;;

let bind parser transform chars =
  match parser chars with
  | Parsed (a, rest) -> transform a rest
  | ParseError _ as e -> e
;;

let ( >>= ) = bind
let ( let* ) = bind
let string_of_chars chars = chars |> List.to_seq |> String.of_seq
let chars_of_string string = string |> String.to_seq |> List.of_seq

let satisfy predicate = function
  | h :: tl when predicate h -> return h tl
  | h :: tl -> fail (SyntaxError (string_of_chars (h :: tl))) tl
  | [] -> fail UnexpectedEnd []
;;

let char c = satisfy (( = ) c)

let ( <*> ) parser_func parser chars =
  match parser_func chars with
  | ParseError _ as e -> e
  | Parsed (f, tail) ->
    (match parser tail with
     | ParseError _ as e -> e
     | Parsed (a, tl) -> return (f a) tl)
;;

let ( <|> ) left right chars =
  match left chars with
  | ParseError _ -> right chars
  | Parsed _ as res -> res
;;

let ( >>| ) parser transform chars =
  match parser chars with
  | ParseError _ as e -> e
  | Parsed (x, tl) -> return (transform x) tl
;;

let choice parsers = List.fold_left ( <|> ) (fail UnexpectedEnd) parsers
let ( *> ) left right = left >>= fun _ -> right
let ( <* ) left right = left >>= fun a -> right >>= fun _ -> return a

let rec zero_many parser chars =
  match parser chars with
  | ParseError _ -> return [] chars
  | Parsed (a, rest) ->
    (let* a_tail = zero_many parser in
     return (a :: a_tail))
      rest
;;

let one_many parser =
  let* x = parser in
  let* xs = zero_many parser in
  return (x :: xs)
;;

let alpha =
  satisfy (function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false)
;;

let digit =
  satisfy (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let chars string =
  match chars_of_string string with
  | [] -> fail UnexpectedEnd
  | c :: rest -> List.fold_left ( *> ) (char c) (List.map char rest)
;;

let reserved = function
  | "let" -> true
  | "rec" -> true
  | "in" -> true
  | "if" -> true
  | "then" -> true
  | "else" -> true
  | "fun" -> true
  | "false" -> true
  | "true" -> true
  | "mod" -> true
  | _ -> false
;;

let ws0 = zero_many (char ' ' <|> char '\t' <|> char '\n')
let ws1 = one_many (char ' ' <|> char '\t' <|> char '\n')
let parens parser = ws0 *> char '(' *> parser <* ws0 <* char ')'

let identifier =
  let wildcard =
    let* _ = char '_' in
    return Ast.Wildcard
  in
  let real =
    let* x = alpha in
    let* xs = zero_many (alpha <|> digit <|> char '_') in
    let name = string_of_chars (x :: xs) in
    if reserved name then fail (SyntaxError name) else return (Ast.Real name)
  in
  wildcard <|> real
;;

let term =
  let integer =
    let* chars = one_many digit in
    return (Ast.Int (int_of_string (string_of_chars chars)))
  in
  let boolean =
    let parser_true = chars "false" >>| fun _ -> Ast.Bool false in
    let parser_false = chars "true" >>| fun _ -> Ast.Bool true in
    parser_true <|> parser_false
  in
  let variable =
    let* name = identifier in
    return (Ast.Var name)
  in
  let unit =
    let* _ = chars "()" in
    return Ast.Unit
  in
  integer <|> boolean <|> variable <|> unit
;;

let arguments parser = one_many (ws1 *> parser)

let apply_precedence parser operators =
  let length = Array.length operators in
  let rec helper level =
    if level >= length
    then parser
    else (
      let xs = operators.(level) in
      return (List.fold_left (fun acc (op, r) -> op acc r))
      <*> helper (level + 1)
      <*> zero_many
            (choice
               (List.map
                  (fun (sign, left) ->
                    let* right = ws0 *> sign *> ws0 *> helper (level + 1) in
                    return (left, right))
                  xs)))
  in
  helper 0
;;

let atom =
  let binary typ left right = Ast.BinaryOp (typ, left, right) in
  let unary operation typ parser =
    let* _ = chars operation in
    let* expr = parser in
    return (Ast.UnaryOp (typ, expr))
  in
  let neg = unary "-" Ast.Neg in
  let not = unary "!" Ast.Not in
  let tuple parser =
    let* _ = char '(' in
    let* fst = parser in
    let* rest = one_many (ws0 *> char ',' *> ws0 *> parser) in
    let* _ = char ')' in
    return (Ast.Tuple (fst, List.hd rest, List.tl rest))
  in
  let application parser1 parser2 =
    let* _ = char '(' in
    let* name = parser1 in
    let* args = arguments parser2 in
    let* _ = char ')' in
    return (List.fold_left (fun left right -> Ast.Application (left, right)) name args)
  in
  let abstraction parser =
    let* _ = chars "fun" in
    let* args = arguments identifier in
    let* _ = ws0 *> chars "->" in
    let* expr = ws0 *> parser in
    return (List.fold_right (fun left right -> Ast.Abstraction (left, right)) args expr)
  in
  let if_expr parser1 parser2 =
    let* _ = chars "if" in
    let* cond = ws1 *> parser1 in
    let* _ = ws1 *> chars "then" in
    let* on_true = ws1 *> parser2 in
    let* _ = ws1 *> chars "else" in
    let* on_false = ws1 *> parser2 in
    return (Ast.IfThenElse (cond, on_true, on_false))
  in
  let letrec mnemo rec_flag parser1 parser2 =
    let* _ = chars mnemo in
    let* name = ws1 *> identifier in
    let* _ = ws0 *> char '=' in
    let* expr1 = ws0 *> parser1 in
    let* _ = ws1 *> chars "in" in
    let* expr2 = ws1 *> parser2 in
    return (Ast.LetExpr (rec_flag, name, expr1, expr2))
  in
  let let_expr = letrec "let" Ast.Let in
  let let_rec_expr = letrec "let rec" Ast.LetRec in
  fix (fun atom ->
    let atom_fix =
      fix (fun inner ->
        term
        <|> choice [ neg inner; not inner ]
        <|> application atom inner
        <|> abstraction inner
        <|> tuple inner
        <|> if_expr atom inner
        <|> let_expr atom inner
        <|> let_rec_expr atom inner
        <|> parens atom)
    in
    apply_precedence
      atom_fix
      [| [ chars "&&", binary Ast.And ]
       ; [ chars "||", binary Ast.Or ]
       ; [ char '=', binary Ast.Equal
         ; chars "<>", binary Ast.NotEqual
         ; char '<', binary Ast.Less
         ; chars "<=", binary Ast.LessEqual
         ; char '>', binary Ast.Greater
         ; chars ">=", binary Ast.GreaterEqual
         ]
       ; [ char '+', binary Ast.Add; char '-', binary Ast.Sub ]
       ; [ chars "mod", binary Ast.Mod
         ; char '*', binary Ast.Mul
         ; char '/', binary Ast.Div
         ]
       ; [ (chars "@@", fun left right -> Ast.Application (left, right)) ]
      |])
;;

let wrap_result = function
  | Parsed (a, rest) when rest = [] -> Parsed (a, rest)
  | Parsed (_, rest) -> ParseError (UnexpectedRest (string_of_chars rest))
  | ParseError _ as e -> e
;;

let parse_line line =
  let expr = ws0 *> atom <* ws0 in
  chars_of_string line |> expr |> wrap_result
;;

let show_error = function
  | UnexpectedEnd -> "Unexpected end of input"
  | UnexpectedRest s -> Format.sprintf {|Unparsed symbols: "%s"|} s
  | SyntaxError s -> Format.sprintf {|Syntax error: "%s"|} s
;;

let show_result = function
  | Parsed (ast, _) -> Ast.show_ast ast
  | ParseError e -> show_error e
;;
