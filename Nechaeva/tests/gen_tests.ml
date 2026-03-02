[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck.Gen
open Lambda_lib
open Ast

let ident_start_gen =
  oneof
    [ map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
    ; map Char.chr (int_range (Char.code 'A') (Char.code 'Z'))
    ; return '_'
    ]
;;

let ident_cont_gen =
  oneof [ ident_start_gen; map Char.chr (int_range (Char.code '0') (Char.code '9')) ]
;;

let gen_name =
  ident_start_gen
  >>= fun first ->
  list ident_cont_gen
  >>= fun rest ->
  let name = String.of_seq (List.to_seq (first :: rest)) in
  match name with
  | "let" | "in" | "rec" | "if" | "then" | "else" | "fun" -> return (name ^ "_id")
  | _ -> return name
;;

let gen_int = oneof [ int_range 0 100; int_range 1 1000 ]
let gen_constant = map (fun i -> Int i) gen_int

type gen_rec_flag = rec_flag =
  | NonRecursive
  | Recursive
[@@deriving qcheck]

type gen_binop = binop =
  | Plus
  | Minus
  | Mul
  | Div
[@@deriving qcheck]

type gen_compop = compop =
  | Equal
  | NotEqual
  | Less
  | LessEq
  | Greater
  | GreaterEq
[@@deriving qcheck]

type gen_unop = unop = Neg [@@deriving qcheck]

type gen_expr = expr =
  | Const of (constant[@gen gen_constant])
  | Var of (string[@gen gen_name])
  | Abs of (string list[@gen list_size (int_range 1 3) gen_name]) * gen_expr
  | App of gen_expr * gen_expr
  | Let of gen_rec_flag * (string[@gen gen_name]) * gen_expr * gen_expr
  | BinOp of gen_binop * gen_expr * gen_expr
  | UnOp of gen_unop * gen_expr
  | Comp of gen_compop * gen_expr * gen_expr
  | If of gen_expr * gen_expr * gen_expr
[@@deriving qcheck]

(* let gen_expr =
   let rec aux depth =
   let open Gen in
   if depth = 0
   then oneof [ map (fun c -> Const c) gen_constant; map (fun n -> Var n) gen_name ]
   else (
   let deep = aux (depth - 1) in
   let medium = aux (depth / 2) in
   let simple = aux (depth / 3) in
   frequency
   [ 4, aux 0
        ; 2, map3 (fun op e1 e2 -> BinOp (op, e1, e2)) gen_binop medium medium
        ; 2, map3 (fun op e1 e2 -> Comp (op, e1, e2)) gen_compop medium medium
        ; 2, map2 (fun f arg -> App (f, arg)) deep simple
        ; 2, map3 (fun c t e -> If (c, t, e)) medium deep deep
        ; ( 1
          , let* params = list_size (int_range 1 3) gen_name in
            let* body = deep in
            return (Abs (params, body)) )
        ; ( 1
          , let* flag = gen_rec_flag in
            let* name = gen_name in
            let* value = medium in
            let* body = deep in
            return (Let (flag, name, value, body)) )
        ; ( 1
          , let* e = simple in
            return (UnOp (Neg, e)) )
        ])
   in
   Gen.sized (fun size ->
   let depth = min (size / 2) 5 in
   aux depth)
   ;; *)

let gen_expr =
  sized (fun size ->
    let depth = min (size / 2) 5 in
    gen_gen_expr_sized depth)
;;

let arb_expr = QCheck.make ~print:Printast.string_of_expr gen_expr
let arb_program = QCheck.make ~print:(fun s -> s) (map Printast.string_of_expr gen_expr)

let gen_invalid_program =
  oneof
    [ pure "let x = "
    ; pure "if x > 5 then"
    ; pure "fun x ->"
    ; pure "x + "
    ; pure "("
    ; map (fun n -> Printf.sprintf "%d + + %d" n n) small_int
    ; map (fun n -> Printf.sprintf "%d * * %d" n n) small_int
    ; map
        (fun kw -> Printf.sprintf "let %s = 5 in x" kw)
        (oneofl [ "let"; "in"; "rec"; "if"; "then"; "else"; "fun" ])
    ; pure "fun x .. -> x"
    ; pure "let let x = 5 in x"
    ; pure "if if x then 1 else 0"
    ; pure ""
    ; pure "   "
    ; pure "123abc"
    ; pure "123ABC"
    ; pure "123xyz"
    ; pure "123XYZ"
    ; pure "*y"
    ; pure "x + y *"
    ; pure "* x y"
    ]
;;

let arb_invalid_program = QCheck.make ~print:(fun s -> s) gen_invalid_program

let test_roundtrip =
  QCheck.Test.make ~name:"Round-trip parsing" ~count:1000 arb_expr (fun expr ->
    let printed = Printast.string_of_expr expr in
    match Parser.parse printed with
    | Ok parsed -> expr = parsed
    | Error (`Parsing_error msg) ->
      Printf.eprintf "Parse error on: %s\nError: %s\n" printed msg;
      false)
;;

let test_printer_safety =
  QCheck.Test.make ~name:"Printer doesn't crash" ~count:1000 arb_expr (fun expr ->
    try
      let _ = Printast.string_of_expr expr in
      true
    with
    | exn ->
      Printf.eprintf
        "Printer crashed on expr with exception: %s\n"
        (Printexc.to_string exn);
      false)
;;

let test_invalid_programs =
  QCheck.Test.make
    ~name:"Parser rejects invalid programs"
    ~count:500
    arb_invalid_program
    (fun invalid_str ->
       match Parser.parse invalid_str with
       | Ok ast ->
         Printf.eprintf "Parser accepted invalid input: %s\n" invalid_str;
         Printf.eprintf "Parsed as: %s\n" (Printast.string_of_expr ast);
         false
       | Error (`Parsing_error _) -> true)
;;

let test_idempotent =
  QCheck.Test.make ~name:"Idempotent parsing" ~count:1000 arb_program (fun program_str ->
    match Parser.parse program_str with
    | Ok ast1 ->
      let printed1 = Printast.string_of_expr ast1 in
      (match Parser.parse printed1 with
       | Ok ast2 -> ast1 = ast2
       | Error (`Parsing_error msg) ->
         Printf.eprintf
           "Second parse failed: %s -> %s\nError: %s\n"
           program_str
           printed1
           msg;
         false)
    | Error (`Parsing_error msg) ->
      Printf.eprintf "First parse failed: %s\nError: %s\n" program_str msg;
      false)
;;

QCheck_runner.run_tests
  ~verbose:true
  [ test_roundtrip; test_printer_safety; test_invalid_programs; test_idempotent ]
