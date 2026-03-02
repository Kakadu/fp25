(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type qf_mltype =
  | Basetype of string
  | Arrowtype of qf_mltype * qf_mltype
  | Vartype of int
  | Prod of qf_mltype * qf_mltype
  | Sum of qf_mltype * qf_mltype
[@@deriving eq]

let qf_mltype_gen =
  QCheck.Gen.oneofl
    [ Basetype "int"; Basetype "bool"; Arrowtype (Basetype "int", Basetype "bool") ]
;;

type mltype = qf_mltype * int list

let qf_mltype_to_string (t : qf_mltype) =
  let rec helper = function
    | Basetype s -> s
    | Arrowtype (s1, s2) -> "(" ^ helper s1 ^ " -> " ^ helper s2 ^ ")"
    | Prod (s1, s2) -> "(" ^ helper s1 ^ " * " ^ helper s2 ^ ")"
    | Sum (s1, s2) -> "(" ^ helper s1 ^ " + " ^ helper s2 ^ ")"
    | Vartype i -> "bv" ^ string_of_int i
  in
  helper t
;;

let mltype_to_string (t : mltype) = qf_mltype_to_string (fst t)

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
  ; "int"
  ; "bool"
  ; "of"
  ; ""
  ]
;;

let ident_gen =
  QCheck.Gen.map
    (fun x -> if List.mem x reserved then "reserved" else x)
    (QCheck.Gen.string_small_of (QCheck.Gen.char_range 'a' 'z'))
;;

let constr_gen =
  QCheck.Gen.map
    (fun x -> "A" ^ x)
    (QCheck.Gen.string_small_of (QCheck.Gen.char_range 'a' 'z'))
;;

type identifier = (string[@gen ident_gen]) [@@deriving eq, qcheck]
type constructor = (string[@gen constr_gen]) [@@deriving eq, qcheck]
type integer = (int[@gen QCheck.Gen.small_nat]) [@@deriving eq, qcheck]

type mlterm =
  | Var of identifier
  | Constr of constructor
  | Int of integer
  | Bool of bool
  | Unit
  | ITE of mlterm * mlterm * mlterm
  | Let of identifier * mlterm * mlterm
  | LetRec of identifier * mlterm * mlterm
  | LetExc of constructor * (qf_mltype[@gen qf_mltype_gen]) * mlterm
  | App of mlterm * mlterm
  | Fun of identifier * mlterm
  | Pair of mlterm * mlterm
  | Match of mlterm * identifier * mlterm * identifier * mlterm
  | Try of
      mlterm
      * ((constructor * identifier * mlterm) list
        [@gen
          QCheck.Gen.list_size
            (QCheck.Gen.int_range 1 5)
            (QCheck.Gen.map
               (fun (gen0, gen1, gen2) -> gen0, gen1, gen2)
               (QCheck.Gen.triple
                  gen_constructor
                  gen_identifier
                  (gen_mlterm_sized (n / 2))))])
[@@deriving eq, qcheck]

let rec mlterm_to_string : mlterm -> string = function
  | Var x -> x
  | Constr c -> c
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
  | LetExc (v, tp, t) ->
    "let exception " ^ v ^ " of " ^ qf_mltype_to_string tp ^ " in " ^ mlterm_to_string t
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
        (List.map (fun (s, v, t) -> "| " ^ s ^ " " ^ v ^ " -> " ^ mlterm_to_string t) l)
    ^ ")"
;;
