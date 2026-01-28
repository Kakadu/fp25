(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier =
  (string
  [@gen
    QCheck.Gen.string_size
      ~gen:(QCheck.Gen.char_range 'a' 'z')
      (QCheck.Gen.int_range 5 10)])
[@@deriving eq, qcheck]

type integer = (int[@gen QCheck.Gen.int_range 0 1024]) [@@deriving eq, qcheck]

type mlterm =
  | Var of identifier
  | Int of integer
  | Bool of bool
  | Unit
  | ITE of mlterm * mlterm * mlterm
  | Let of identifier * mlterm * mlterm
  | LetRec of identifier * mlterm * mlterm
  | App of mlterm * mlterm
  | Fun of identifier * mlterm
  | Pair of mlterm * mlterm
  | Match of mlterm * identifier * mlterm * identifier * mlterm
[@@deriving eq, qcheck]

let rec mlterm_to_string : mlterm -> string = function
  | Var x -> x
  | Int i -> Printf.sprintf "%d" i
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "()"
  | ITE (c, th, e) ->
    "(if "
    ^ mlterm_to_string c
    ^ " then "
    ^ mlterm_to_string th
    ^ " else "
    ^ mlterm_to_string e
    ^ ")"
  | Let (v, t1, t2) ->
    "let " ^ v ^ "=" ^ mlterm_to_string t1 ^ " in " ^ mlterm_to_string t2
  | LetRec (v, t1, t2) ->
    "let rec " ^ v ^ "=" ^ mlterm_to_string t1 ^ " in " ^ mlterm_to_string t2
  | App (t1, t2) -> "(" ^ mlterm_to_string t1 ^ " " ^ mlterm_to_string t2 ^ ")"
  | Fun (x, t2) -> "fun " ^ x ^ " -> " ^ mlterm_to_string t2
  | Pair (t1, t2) -> "(" ^ mlterm_to_string t1 ^ ", " ^ mlterm_to_string t2 ^ ")"
  | Match (t, v1, t1, v2, t2) ->
    "(match "
    ^ mlterm_to_string t
    ^ " with | inl "
    ^ v1
    ^ " -> "
    ^ mlterm_to_string t1
    ^ "| inr "
    ^ v2
    ^ " -> "
    ^ mlterm_to_string t2
    ^ ")"
;;
