(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


open Angstrom
open Ast

(** polymorphic variant with one constructor **)
type error = [ `Parsing_error of string ]

(** spaces and lexis **)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let spaces = skip_while is_space
let spaces1 = satisfy is_space *> spaces

let lexeme p = spaces *> p
(** parser for one specific character c **)
let sym c = lexeme (char c)
(** parser for one specific key word s **)
let kwd s = lexeme (string s)

(** predicate function for identifiers **)
let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(** predicate functions for identifiers **)
let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

(** list of key words **)
let keywords =
  [ "let"; "in"; "fun"; "rec"; "if"; "then"; "else"; "true"; "false" ]

let is_keyword s = List.mem s keywords


(** 
in parse case: "foo123"
 -> Ok "foo123" 
in parse case: "let"
 -> Error "keyword cannot be used as an identifier" **)
let identifier : name Angstrom.t =
  let open Angstrom in
  spaces
  *> (lift2 
        (fun c cs ->
           let buf = Buffer.create (1 + List.length cs) in
           Buffer.add_char buf c;
           List.iter (Buffer.add_char buf) cs;
           Buffer.contents buf)
        (satisfy is_ident_start)
        (many (satisfy is_ident_char)))
  >>= fun s ->
  if is_keyword s
  then fail "keyword cannot be used as an identifier"
  else return s

(** consts **)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let integer : int Angstrom.t =
  let open Angstrom in
  spaces
  *> (option 1
        (char '-' *> return (-1))
     >>= fun sign ->
     take_while1 is_digit >>| fun digits ->
     sign * int_of_string digits)

let const_int =
  integer >>| fun n -> Const (Int n)

let const_unit =
  lexeme (string "()") *> return (Const Unit)

let const_bool =
  (kwd "true" *> return (Const (Int 1)))
  <|> (kwd "false" *> return (Const (Int 0)))

(** variables **)

let var_expr : expression Angstrom.t =
  identifier >>| fun x -> Var x

let parens p = sym '(' *> p <* sym ')'

(** operations **)

let parse_op_add =
  sym '+' *> return OpAdd

let parse_op_sub =
  sym '-' *> return OpSub

let parse_op_mul =
  sym '*' *> return OpMul

let parse_op_div =
  sym '/' *> return OpDiv

let parse_cmp_op : operation_id Angstrom.t =
  let open Angstrom in
  spaces
  *> choice
       [ string ">=" *> return OpGte
       ; string "<=" *> return OpLte
       ; string "="  *> return OpEq
       ; string ">"  *> return OpGt
       ; string "<"  *> return OpLt
       ]

(** syntax suger for fun x y -> e **)
let curry_fun (args : name list) (body : expression) : expression =
  List.fold_right (fun x e -> Fun (x, e)) args body

(** left-associative chain combinator **)
let chainl1 p op =
  let open Angstrom in
  let rec loop acc =
    (lift2 (fun f x -> f acc x) op p >>= loop) <|> return acc
  in
  p >>= loop

(** basic grammar levels **)
let expr : expression Angstrom.t =
  fix (fun expr ->

    (** fun x y -> e **)
    let lambda =
      let args =
        kwd "fun" *> spaces
        *> many1 identifier
        <* spaces
        <* kwd "->"
      in
      args >>= fun xs ->
      expr >>| fun body -> curry_fun xs body
    in

    (** basic atom without application **)
    let atom0 =
      choice
        [ const_unit
        ; const_bool
        ; const_int
        ; lambda
        ; var_expr
        ; parens expr
        ]
    in

    (** application: f a b c **)
    let application =
      let open Angstrom in
      atom0
      >>= fun f ->
      many1 (spaces1 *> atom0)
      >>| fun args ->
      List.fold_left (fun acc a -> App (acc, a)) f args
    in

    let atom =
      application <|> atom0
    in

    (** level * and / **)
    let mul_div =
      let op =
        (parse_op_mul <|> parse_op_div)
        >>| fun op l r -> BinOp (op, l, r)
      in
      chainl1 atom op
    in

    (** level + and - **)
    let add_sub =
      let op =
        (parse_op_add <|> parse_op_sub)
        >>| fun op l r -> BinOp (op, l, r)
      in
      chainl1 mul_div op
    in

    (** comparisons we allow only one comparison in the chain:
       a + b < c * d **)
    let cmp_level =
      let open Angstrom in
      add_sub
      >>= fun left ->
      option left
        (parse_cmp_op >>= fun op ->
         add_sub >>| fun right -> BinOp (op, left, right))
    in

    (** if ... then ... else ... **)
    let if_expr =
      let open Angstrom in
      kwd "if" *> cmp_level
      >>= fun cond ->
      kwd "then" *> expr
      >>= fun thn ->
      option None
        (kwd "else" *> expr >>| fun e -> Some e)
      >>| fun els -> If (cond, thn, els)
    in

    (** let / let rec **)
    let let_expr =
      let open Angstrom in
      kwd "let"
      *> spaces
      *> option NonRec (kwd "rec" *> return Rec)
      >>= fun kind ->
      identifier
      >>= fun name ->
      (** function parameters: let f x y = e **)
      many identifier
      >>= fun args ->
      sym '=' *> expr
      >>= fun bound ->
      let value = curry_fun args bound in
      option None
        (kwd "in" *> expr >>| fun body -> Some body)
      >>= fun body_opt ->
      let scope =
        match body_opt with
        | None   -> GlobalVar
        | Some _ -> LocalVar
      in
      return (Let (scope, kind, name, value, body_opt))
    in

    (** top level: first let/if, then regular expressions **)
    choice
      [ let_expr
      ; if_expr
      ; cmp_level
      ]
  )

(** wrapper over angshtorm **)

let parse (s : string) : (expression, error) result =
  match parse_string ~consume:Consume.All expr s with
  | Ok e -> Ok e
  | Error msg -> Error (`Parsing_error msg)

let pp_error fmt (`Parsing_error msg) =
  Format.fprintf fmt "Parse error: %s" msg
