[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let rec pprint = function
  | Ast.Const x -> string_of_int x
  | Ast.Ident x -> x
  | Ast.Binexpr (op, l, r) ->
    let op_str =
      match op with
      | Ast.Plus -> "+"
      | Ast.Minus -> "-"
      | Ast.Mul -> "*"
      | Ast.Div -> "/"
      | Ast.Eq -> "="
      | Ast.Bi -> ">"
      | Ast.Neq -> "!="
      | Ast.Le -> "<"
    in
    String.concat "" [ "("; pprint l; ") "; op_str; " ("; pprint r; ")" ]
  | Ast.Ite (cond, tb, eb) ->
    String.concat "" [ "if "; pprint cond; " then "; pprint tb; " else "; pprint eb ]
  | Ast.Let (b, name, letexpr, inexpr) ->
    let rec_flag = if b then "rec " else "" in
    String.concat
      ""
      [ "let "; rec_flag; name; " = "; pprint letexpr; " in "; pprint inexpr ]
  | Ast.Abs (arg, f) -> String.concat "" [ "fun "; arg; " -> "; pprint f ]
  | Ast.App app ->
    (match app with
     | Var (s, e) -> String.concat "" [ s; " ("; pprint e; ")" ]
     | Fun (s, e, arg) ->
       String.concat "" [ "(fun "; s; " -> "; pprint e; ") "; pprint arg ]
     | Application (some, e) ->
       String.concat "" [ "("; pprint (Ast.App some); ") "; " ("; pprint e; ")" ])
;;
