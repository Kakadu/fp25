(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let reserved =
  [ "let"
  ; "rec"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "fun"
  ; "true"
  ; "false"
  ; "try"
  ; "match"
  ; "with"
  ; "exception"
  ; "inr"
  ; "inl"
  ; "not"
  ; ""
  ]
;;

let ident_gen =
  QCheck.Gen.map
    (fun x -> if List.mem x reserved then "reserved" else x)
    (QCheck.Gen.string_small_of (QCheck.Gen.char_range 'a' 'z'))
;;

type identifier = (string[@gen ident_gen]) [@@deriving eq, qcheck]
type integer = (int[@gen QCheck.Gen.int_pos]) [@@deriving eq, qcheck]

type mlterm =
  | Var of identifier
  | Int of integer
  | Bool of bool
  | Unit
  | ITE of mlterm * mlterm * mlterm
  | Let of identifier * mlterm * mlterm
  | LetRec of identifier * mlterm * mlterm
  | LetExc of identifier * mlterm
  | App of mlterm * mlterm
  | Fun of identifier * mlterm
  | Pair of mlterm * mlterm
  | Match of mlterm * identifier * mlterm * identifier * mlterm
  | Try of
      mlterm
      * ((identifier * mlterm) list
        [@gen
          QCheck.Gen.list_size
            (QCheck.Gen.int_range 1 5)
            (QCheck.Gen.map
               (fun (gen0, gen1) -> gen0, gen1)
               (QCheck.Gen.pair gen_identifier (gen_mlterm_sized (n / 2))))])
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
  | LetExc (v, t) -> "let exception " ^ v ^ " in " ^ mlterm_to_string t
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
  | Try (t, l) ->
    "(try "
    ^ mlterm_to_string t
    ^ " with "
    ^ String.concat
        " "
        (List.map (fun (s, t) -> "| " ^ s ^ " -> " ^ mlterm_to_string t) l)
    ^ ")"
;;
