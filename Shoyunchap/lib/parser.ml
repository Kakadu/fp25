(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* map operator for angstrom *)
let ( let+ ) = Angstrom.( >>| )

(* polymorphic variant with one constructor *)
type error = [ `Parsing_error of string ]

(* spaces and lexis *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let spaces1 = satisfy is_space *> spaces
let lexeme p = spaces *> p

(* parser for one specific character c *)
let sym c = lexeme (char c)

(* parser for one specific key word s *)
let kwd s = lexeme (string s)

(* predicate function for identifiers *)
let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

(* predicate functions for identifiers *)
let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

(* list of key words *)
let keywords = [ "let"; "in"; "fun"; "rec"; "if"; "then"; "else"; "true"; "false" ]
let is_keyword s = List.mem s keywords

(*
   in parse case: "foo123"
   -> Ok "foo123"
   in parse case: "let"
   -> Error "keyword cannot be used as an identifier" *)
let identifier : name Angstrom.t =
  let open Angstrom in
  let raw_ident =
    let* first = satisfy is_ident_start in
    let* rest = take_while is_ident_char in
    return (String.make 1 first ^ rest)
  in
  let* name = lexeme raw_ident in
  if is_keyword name then fail "keyword cannot be used as an identifier" else return name
;;

(* consts *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer : int Angstrom.t =
  let open Angstrom in
  spaces
  *>
  let* sign = option 1 (char '-' *> return (-1)) in
  let* digits = take_while1 is_digit in
  return (sign * int_of_string digits)
;;

let const_int =
  let+ n = integer in
  Const (Int n)
;;

let const_unit = lexeme (string "()") *> return (Const Unit)

let const_bool =
  kwd "true" *> return (Const (Int 1)) <|> kwd "false" *> return (Const (Int 0))
;;

(* variables *)
let var_expr : expression Angstrom.t =
  let+ x = identifier in
  Var x
;;

let parens p = sym '(' *> p <* sym ')'

(* operations *)

let parse_op_add = sym '+' *> return OpAdd
let parse_op_sub = sym '-' *> return OpSub
let parse_op_mul = sym '*' *> return OpMul
let parse_op_div = sym '/' *> return OpDiv

let parse_cmp_op : operation_id Angstrom.t =
  let open Angstrom in
  spaces
  *> choice
       [ string "<=" *> return OpLte
       ; string ">=" *> return OpGte
       ; string ">" *> return OpGt
       ; string "<" *> return OpLt
       ; string "=" *> return OpEq
       ]
;;

(* syntax sugar for fun x y -> e
   example result: Fun ("x", Fun ("y", Fun ("z", body)))
   with args : [x, y, z]
*)
let curry_fun (args : name list) (body : expression) : expression =
  List.fold_right (fun x e -> Fun (x, e)) args body
;;

(* left-associative chain combinator
   ((1 + 2) + 3)
   with "1+2+3"
*)
let chainl1 p op =
  let open Angstrom in
  let rec loop acc =
    (let* apply = op in
     let* operand = p in
     loop (apply acc operand))
    <|> return acc
  in
  let* first = p in
  loop first
;;

(* basic grammar levels *)
let expr : expression Angstrom.t =
  fix (fun expr ->
    (* fun x y -> e *)
    let lambda =
      let args = kwd "fun" *> spaces *> many1 identifier <* spaces <* kwd "->" in
      let* xs = args in
      let+ body = expr in
      curry_fun xs body
    in
    (* basic atom without application *)
    let atom0 =
      choice [ const_unit; const_bool; const_int; lambda; var_expr; parens expr ]
    in
    (* application: f a b c
       example input: f x y
       output: App (App (Var "f", Var "x"), Var "y") *)
    let application =
      let open Angstrom in
      let* f = atom0 in
      let+ args = many1 (spaces1 *> atom0) in
      List.fold_left (fun acc a -> App (acc, a)) f args
    in
    let atom = application <|> atom0 in
    (* level * and / *)
    let mul_div =
      let op =
        let+ op = parse_op_mul <|> parse_op_div in
        fun l r -> BinOp (op, l, r)
      in
      chainl1 atom op
    in
    (* level + and - *)
    let add_sub =
      let op =
        let+ op = parse_op_add <|> parse_op_sub in
        fun l r -> BinOp (op, l, r)
      in
      chainl1 mul_div op
    in
    (* comparisons we allow only one comparison in the chain:
       a + b < c - d
    *)
    let cmp_level =
      let open Angstrom in
      let* left = add_sub in
      option
        left
        (let* op = parse_cmp_op in
         let+ right = add_sub in
         BinOp (op, left, right))
    in
    (* if ... then ... else ... *)
    let if_expr =
      let open Angstrom in
      let* cond = kwd "if" *> cmp_level in
      let* thn = kwd "then" *> expr in
      let* els =
        option
          None
          (let+ e = kwd "else" *> expr in
           Some e)
      in
      return (If (cond, thn, els))
    in
    (* let / let rec *)
    let let_expr =
      let open Angstrom in
      let* kind = kwd "let" *> spaces *> option NonRec (kwd "rec" *> return Rec) in
      let* name = identifier in
      (* function parameters: let f x y = e *)
      let* args = many identifier in
      let* bound = sym '=' *> expr in
      let value = curry_fun args bound in
      let* body_opt =
        option
          None
          (let+ body = kwd "in" *> expr in
           Some body)
      in
      let scope =
        match body_opt with
        | None -> GlobalVar
        | Some _ -> LocalVar
      in
      return (Let (scope, kind, name, value, body_opt))
    in
    (* top level: first let/if, then regular expressions *)
    choice [ let_expr; if_expr; cmp_level ])
;;

(* wrapper over angshtorm *)

let parse (s : string) : (expression, error) result =
  match parse_string ~consume:Consume.All expr s with
  | Ok e -> Ok e
  | Error msg -> Error (`Parsing_error msg)
;;

let pp_error fmt (`Parsing_error msg) = Format.fprintf fmt "Parse error: %s" msg
