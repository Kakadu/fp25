open Lambda_lib
open Parser

let rec fact n = if n = 1 then 1 else n * fact (n - 1)
let%test _ = fact 5 = 120

(* test number parsing *)
(* successfully parsed numbers *)
let num str =
  match Angstrom.parse_string parse_number ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse number")
;;

let%test _ = Result.get_ok (parse "100") = Ast.Const 100
let%test _ = Result.get_ok (parse "1") = Ast.Const 1
let%test _ = Result.get_ok (parse "-1") = Ast.Const (-1)
let%test _ = Result.get_ok (parse "-102929") = Ast.Const (-102929)

(* no success *)
let%test _ = Result.get_error (parse "-") = `Parsing_error "Failed to parse"
let%test _ = Result.get_error (parse "1-000") = `Parsing_error "Failed to parse"
let%test _ = Result.get_error (parse "--1") = `Parsing_error "Failed to parse"

(* test ident parsing *)
(* successfully parsed identifiers *)
let ident str =
  match Angstrom.parse_string parse_varname ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ = Result.get_ok (ident "aaa") = Ast.Ident "aaa"
let%test _ = Result.get_ok (ident "1a") = Ast.Ident "1a"
let%test _ = Result.get_ok (ident "a1") = Ast.Ident "a1"

(* no success *)
let%test _ = Result.get_error (ident "1") = `Parsing_error "Failed to parse ident"
let%test _ = Result.get_error (ident "") = `Parsing_error "Failed to parse ident"

(* test parser of arithmetics *)
let algebr str =
  match Angstrom.parse_string parse_arithm ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ =
  Result.get_ok (algebr "1+2") = Ast.Binexpr (Ast.Plus, Ast.Const 1, Ast.Const 2)
;;

let%test _ =
  Result.get_ok (algebr "2*a") = Ast.Binexpr (Ast.Mul, Ast.Const 2, Ast.Ident "a")
;;

let%test _ =
  Result.get_ok (algebr "1+(2+3)")
  = Ast.Binexpr (Ast.Plus, Ast.Const 1, Ast.Binexpr (Ast.Plus, Ast.Const 2, Ast.Const 3))
;;

let%test _ =
  Result.get_ok (algebr "a*b+c")
  = Ast.Binexpr
      (Ast.Plus, Ast.Binexpr (Ast.Mul, Ast.Ident "a", Ast.Ident "b"), Ast.Ident "c")
;;

let%test _ = Result.get_ok (algebr "a") = Ast.Ident "a"

(* test some comparison *)
let compr str =
  match Angstrom.parse_string parse_arithm ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ = Result.get_ok (compr "1=2") = Ast.Binexpr (Ast.Eq, Ast.Const 1, Ast.Const 2)

let%test _ =
  Result.get_ok (compr "1*a<2")
  = Ast.Binexpr (Ast.Le, Ast.Binexpr (Ast.Mul, Ast.Const 1, Ast.Ident "a"), Ast.Const 2)
;;
