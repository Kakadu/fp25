open Lambda_lib

let expect_ok src =
  match Parser.parse src with
  | Ok _ -> ()
  | Error e ->
    failwith (Format.asprintf "expected to parse: %s, got %a" src Parser.pp_error e)
;;

let expect_error src =
  match Parser.parse src with
  | Ok _ -> failwith (Printf.sprintf "expected parse failure: %s" src)
  | Error _ -> ()
;;

let base_success =
  [ "42"
  ; "x"
  ; "1 + 2"
  ; "1 + 2 * 3"
  ; "(1 + 2) * 3"
  ; "let x = 5 in x + 1"
  ; "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 4"
  ; "fun x -> x + 1"
  ; "fun x y -> x * y"
  ; "if 1 then 2 else 3"
  ; "let f x = x in f 3"
  ; "let x = fun y -> y in x 2"
  ; "let a = 1 in let b = 2 in a + b"
  ; "let rec f x = if x = 0 then 0 else f (x - 1) in f 3"
  ; "fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) 3"
  ; "true"
  ; "false"
  ; "if true then 1 else 0"
  ; "if false then 1 else 0"
  ; "let x = -5 in x"
  ]
;;

let generated_success =
  let open Printf in
  List.init 120 (fun i ->
      let n = i + 1 in
      [ sprintf "%d + %d" n (n + 1)
      ; sprintf "%d - %d" (n * 2) n
      ; sprintf "%d * %d" n (n + 2)
      ; sprintf "%d / %d" (n * (n + 1)) (n + 1)
      ])
  |> List.flatten
;;

let function_success =
  let open Printf in
  List.init 30 (fun i ->
      let n = i + 1 in
      [ sprintf "fun x -> x + %d" n
      ; sprintf "fun a b -> a * %d + b" n
      ; sprintf "let f x = x + %d in f %d" n (n * 2)
      ; sprintf "let rec f n = if n = 0 then %d else f (n - 1) in f %d" n n
      ; sprintf "let rec g x y = if x = 0 then y else g (x - 1) (y + %d) in g %d %d" n n
          (n * 2)
      ])
  |> List.flatten
;;

let let_chains =
  let open Printf in
  List.init 40 (fun i ->
      let n = i + 1 in
      [ sprintf "let x = %d in x + %d" n n
      ; sprintf "let x = %d in let y = %d in x * y" n (n + 1)
      ; sprintf "let x = %d in let y = x + %d in y * x" n (n + 2)
      ])
  |> List.flatten
;;

let error_cases =
  [ ""
  ; "let"
  ; "1 +"
  ; "(1 + 2"
  ; "1 + 2)"
  ; "if then else"
  ; "fun -> 1"
  ; "let 5 = 3"
  ; "let rec = 1"
  ; "let x = in x"
  ; "fun x y"
  ; "if x"
  ; "let x = 1 in"
  ; "let x = 1 in in"
  ; "let rec f = fun -> 1"
  ]
;;

let () =
  List.iter expect_ok base_success;
  List.iter expect_ok generated_success;
  List.iter expect_ok function_success;
  List.iter expect_ok let_chains;
  List.iter expect_error error_cases
;;
