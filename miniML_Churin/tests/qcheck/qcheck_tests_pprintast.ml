open QCheck
open Lambda_lib.Parser
open Lambda_lib.Ast
open Lambda_lib.Pprintast

let gen_literal =
  let open Gen in
  oneof [ map (fun n -> Integer n) (int_range 0 20); return UnitVal ]
;;

let gen_var =
  let open Gen in
  let letters =
    [ 'a'
    ; 'b'
    ; 'c'
    ; 'd'
    ; 'e'
    ; 'f'
    ; 'g'
    ; 'h'
    ; 'i'
    ; 'j'
    ; 'k'
    ; 'l'
    ; 'm'
    ; 'n'
    ; 'o'
    ; 'p'
    ; 'q'
    ; 'r'
    ; 's'
    ; 't'
    ; 'u'
    ; 'v'
    ; 'w'
    ; 'x'
    ; 'y'
    ; 'z'
    ]
  in
  let gen_start = oneof_list letters in
  let gen_rest = oneof [ return ""; oneof_list [ "1"; "2"; "3"; "_acc"; "_helper" ] ] in
  map2 (fun c s -> String.make 1 c ^ s) gen_start gen_rest
;;

let rec gen_condition depth =
  let open Gen in
  if depth <= 0
  then oneof [ map (fun l -> Lit l) gen_literal; map (fun v -> Var v) gen_var ]
  else (
    let sub = gen_condition (depth - 1) in
    oneof_weighted
      [ 3, map (fun l -> Lit l) gen_literal
      ; 3, map (fun v -> Var v) gen_var
      ; ( 2
        , map3
            (fun op l r -> CmpOp (op, l, r))
            (oneof_list [ Eq; Neq; Lt; Le; Gt; Ge ])
            sub
            sub )
      ; 1, map2 (fun _ e -> UnOp (Negate, e)) (return Negate) sub
      ])
;;

let rec gen_expr depth : expr Gen.t =
  let open Gen in
  let base =
    oneof_weighted
      [ 3, map (fun l -> Lit l) gen_literal; 3, map (fun v -> Var v) gen_var ]
  in
  if depth <= 0
  then base
  else (
    let sub = gen_expr (depth - 1) in
    let small_sub = gen_expr (depth / 2) in
    oneof_weighted
      [ 4, base
      ; 2, map2 (fun op e -> UnOp (op, e)) (return Negate) small_sub
      ; ( 3
        , map3
            (fun op l r -> BinOp (op, l, r))
            (oneof_list [ Add; Sub; Mul; Div ])
            sub
            sub )
      ; ( 3
        , map3
            (fun op l r -> CmpOp (op, l, r))
            (oneof_list [ Eq; Neq; Lt; Le; Gt; Ge ])
            sub
            sub )
      ; 3, map2 (fun f a -> App (f, a)) sub small_sub
      ; 2, map2 (fun p b -> Lam (p, b)) gen_var small_sub
      ; 2, map3 (fun c t e -> If (c, t, e)) (gen_condition 2) small_sub small_sub
      ; ( 2
        , map4
            (fun flag n b body -> Let (flag, n, b, body))
            (oneof_list [ Plain; Recursive ])
            gen_var
            small_sub
            small_sub )
      ])
;;

let arb_expr : expr arbitrary =
  make ~print:(fun e -> to_string ~parens:false e) (gen_expr 5)
;;

let gen_invalid_input : string Gen.t =
  let open Gen in
  let invalid_patterns =
    [ "let"
    ; "let x ="
    ; "let rec"
    ; "fun -> x"
    ; "if then else"
    ; "if x then"
    ; "1 +"
    ; "(1 + 2"
    ; "1 + 2)"
    ; "let x = 5 in"
    ; "fix"
    ; "-"
    ; "= x"
    ; "x ="
    ; "let rec x = 5 in"
    ; "if 5 then 3"
    ; "fun ->"
    ; "let x = in 5"
    ; "x + * y"
    ; "1 + + 2"
    ; "a * / b"
    ; "x = = y"
    ; "x < = y"
    ; "if x then y"
    ; "let x = 5"
    ; "fun x y"
    ; "fix fix"
    ; "let rec"
    ; "let x = in"
    ; "+ 5"
    ; "* 7"
    ; "/ 9"
    ; "if then 1 else 2"
    ; "if x then else 3"
    ]
  in
  oneof_list invalid_patterns
;;

let arb_invalid_input : string arbitrary =
  make ~print:(fun s -> "\"" ^ s ^ "\"") gen_invalid_input
;;

let test_print_parse_identity =
  Test.make ~name:"Pprint.to_string >> Parser.parse = identity" arb_expr (fun expr ->
    let printed = to_string expr in
    Printf.printf "AST: %s\n" (show_expr expr);
    Printf.printf "Printed : %s\n" printed;
    match parse printed with
    | Ok expr' ->
      Printf.printf "Parsed  : %s\n" (show_expr expr');
      let equal = expr = expr' in
      Printf.printf "Equal?  : %b\n" equal;
      equal
    | Error _ ->
      Printf.printf "Parse error!\n";
      false)
;;

let test_print_idempotent =
  Test.make ~name:"Pprint.to_string is idempotent" arb_expr (fun expr ->
    let first = to_string expr in
    match parse first with
    | Ok expr' ->
      let second = to_string expr' in
      let equal_strings = first = second in
      Printf.printf "\n--- Idempotence test ---\n";
      Printf.printf "First : %s\n" first;
      Printf.printf "Second: %s\n" second;
      Printf.printf "Equal? : %b\n" equal_strings;
      equal_strings
    | Error _ ->
      Printf.printf "Parse error in idempotence test!\n";
      false)
;;

let test_parser_rejects_invalid =
  Test.make ~name:"Parser rejects invalid input" arb_invalid_input (fun input ->
    let trimmed = String.trim input in
    if trimmed = ""
    then true
    else (
      match parse trimmed with
      | Ok _ ->
        Printf.printf "\n!!! PARSER ACCEPTED INVALID: '%s'\n" trimmed;
        false
      | Error _ -> true))
;;

let () =
  QCheck_base_runner.run_tests_main
    [ test_print_parse_identity; test_print_idempotent; test_parser_rejects_invalid ]
;;
