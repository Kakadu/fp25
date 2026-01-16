open Lambda_lib

let parse s =
  match Parser.parse s with
  | Ok ast -> ast
  | Error e -> failwith (Format.asprintf "parse failed: %a" Parser.pp_error e)
;;

let expect_int ?steps s expected =
  let ast = parse s in
  let max_steps = Option.value steps ~default:100_000 in
  match Interpret.run ~max_steps ast with
  | Ok (Interpret.IntVal n) when n = expected -> ()
  | Ok v ->
    failwith
      (Printf.sprintf "expected %d, got %s" expected (Interpret.string_of_value v))
    [@coverage off]
  | Error err -> failwith (Interpret.string_of_error err) [@coverage off]
;;

let expect_unit ?steps s =
  let ast = parse s in
  let max_steps = Option.value steps ~default:100_000 in
  match Interpret.run ~max_steps ast with
  | Ok Interpret.UnitVal -> ()
  | Ok v ->
    failwith
      (Printf.sprintf "expected unit, got %s" (Interpret.string_of_value v))
    [@coverage off]
  | Error err -> failwith (Interpret.string_of_error err) [@coverage off]
;;

let expect_error ?steps s expected_msg =
  let ast = parse s in
  let max_steps = Option.value steps ~default:100_000 in
  match Interpret.run ~max_steps ast with
  | Ok v ->
    failwith
      (Printf.sprintf
         "expected error %s, got value %s"
         expected_msg
         (Interpret.string_of_value v)) [@coverage off]
  | Error err ->
    let got = Interpret.string_of_error err in
    if not (String.equal got expected_msg)
    then
      failwith
        (Printf.sprintf "expected error %s, got %s" expected_msg got) [@coverage off]
;;

let () =
  (* literals and basic ops *)
  expect_int "1" 1;
  expect_int "1 + 2" 3;
  expect_int "5 - 7" (-2);
  expect_int "3 * 4" 12;
  expect_int "9 / 3" 3;
  expect_int "8 / 2" 4;
  expect_int "2 = 2" 1;
  expect_int "3 < 2" 0;
  expect_int "3 > 2" 1;
  expect_int "2 < 3" 1;
  expect_int "3 >= 3" 1;
  expect_int "2 <= 1" 0;
  expect_int "3 <= 3" 1;
  expect_int "4 >= 5" 0;
  expect_error "(fun x -> x) + 1" "Not an int: <fun>";
  expect_error "1 + (fun x -> x)" "Not an int: 1";
  expect_error "unknown" "Unbound variable unknown";
  (* let / let rec with and without bodies *)
  expect_int "let x = 4 in x + 1" 5;
  (match Interpret.run (parse "let x = 4") with
   | Ok (Interpret.IntVal 4) -> ()
   | Ok v ->
     failwith
       (Printf.sprintf "expected 4, got %s" (Interpret.string_of_value v)) [@coverage off]
   | Error e -> failwith (Interpret.string_of_error e) [@coverage off]);
  (match Interpret.run (parse "let rec f = fun x -> x") with
   | Ok (Interpret.ClosureVal _) -> ()
   | Ok v ->
     failwith
       (Printf.sprintf "expected closure, got %s" (Interpret.string_of_value v))
     [@coverage off]
   | Error e -> failwith (Interpret.string_of_error e) [@coverage off]);
  (* application and closures *)
  expect_int "(fun x -> x + 1) 4" 5;
  expect_error "5 6" "Not a function: 5";
  expect_int "let f x y = x + y in f 3 4" 7;
  expect_int "let f = fun x -> fun y -> x * y in f 2 5" 10;
  (* if branches, including missing else *)
  expect_int "if 0 then 1 else 2" 2;
  expect_int "if 2 then 1 else 2" 1;
  expect_unit "let r = 4 * 8 in if r < 0 then 8";
  expect_error "if () then 1 else 2" "Not an int: ()";
  (* recursive function via let rec and fix *)
  expect_int "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5" 120;
  expect_int "fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) 5" 120;
  expect_error "fix 5" "fix expects fun self -> fun x -> ...";
  (* builtins *)
  expect_unit "print_int 41";
  expect_error "print_int (fun x -> x)" "Not an int: <fun>";
  expect_unit "print_newline ()";
  ignore (Interpret.string_of_value Interpret.builtin_print_int);
  ignore (Interpret.string_of_value Interpret.builtin_fix);
  (* step limit *)
  expect_error ~steps:0 "1" "Step limit exceeded";
  expect_error ~steps:5 "let rec loop x = loop x in loop 0" "Step limit exceeded";
  (* max_steps_from_env paths *)
  ignore (Interpret.max_steps_from_env 5);
  Unix.putenv "MINIML_MAX_STEPS" "8";
  assert (Interpret.max_steps_from_env 3 = 8);
  Unix.putenv "MINIML_MAX_STEPS" "oops";
  assert (Interpret.max_steps_from_env 3 = 3);
  Unix.putenv "MINIML_MAX_STEPS" "0";
  assert (Interpret.max_steps_from_env 9 = 9);
  Unix.putenv "MINIML_MAX_STEPS" "";
  (* string_of_error exhaustiveness *)
  ignore
    [ Interpret.string_of_error (Unbound_variable "x")
    ; Interpret.string_of_error (Not_a_function Interpret.UnitVal)
    ; Interpret.string_of_error (Not_an_int Interpret.UnitVal)
    ; Interpret.string_of_error Division_by_zero
    ; Interpret.string_of_error Step_limit_exceeded
    ; Interpret.string_of_error Fix_argument_shape
    ];
  (* parse_and_run success and failure *)
  Interpret.parse_and_run "1 + 1";
  Interpret.parse_and_run "let = x";
  Interpret.parse_and_run "1 / 0"
;;
