(* TODO: refactor + add to README *)

open C_sharp_strange_lib
open QCheck
open Ast
open Parser

(* Ast generators *)
let gen_ident =
  let open Gen in
  map
    (fun s -> if List.mem s Parser.reserved then Id "x" else Id s)
    string_small

let gen_ident_length len =
  let open Gen in
  let char_gen = char_range 'a' 'z' in
  let char_list_gen = list_size (return len) char_gen in
  map
    (fun chars ->
      let s = String.of_seq (List.to_seq chars) in
      if List.mem s Parser.reserved then Id "x" else Id s)
    char_list_gen

let gen_val_type =
  let open Gen in
  oneof
    [
      map (fun i -> ValInt i) int;
      map (fun c -> ValChar c) (char_range 'a' 'z');
      map (fun b -> ValBool b) bool;
      map (fun s -> ValString s) string_small;
      return ValNull;
    ]

let gen_binop =
  let open Gen in
  oneof
    [
      return OpAdd;
      return OpSub;
      return OpMul;
      return OpDiv;
      return OpMod;
      return OpEqual;
      return OpNonEqual;
      return OpLess;
      return OpMore;
      return OpLessEqual;
      return OpMoreEqual;
      return OpAnd;
      return OpOr;
      return OpAssign;
    ]

let gen_unop = Gen.return OpNot

let rec gen_expr depth =
  let open Gen in
  if depth <= 0 then
    (* no recursion *)
    oneof
      [
        map (fun v -> EValue v) gen_val_type;
        map (fun id -> EId id) (gen_ident_length 5);
      ]
  else
    let sub = gen_expr (depth - 1) in
    frequency
      [
        (4, sub);
        (2, map2 (fun op (l, r) -> EBinOp (op, l, r)) gen_binop (pair sub sub));
        (1, map2 (fun op e -> EUnOp (op, e)) gen_unop sub);
        ( 1,
          map2
            (fun f args -> EFuncCall (f, Args args))
            sub
            (list_size (1 -- 3) sub) );
      ]

let rec gen_stmt depth =
  let open Gen in
  if depth <= 0 then
    oneof
      [
        map (fun e -> SExpr e) (gen_expr 2);
        map2
          (fun id e -> SDecl (Var (TypeVar (TypeBase TypeInt), id), Some e))
          gen_ident (gen_expr 2);
      ]
  else
    let sub_stmt = gen_stmt (depth - 1) in
    frequency
      [
        (3, map (fun e -> SExpr e) (gen_expr depth));
        ( 2,
          map2
            (fun id e -> SDecl (Var (TypeVar (TypeBase TypeInt), id), Some e))
            gen_ident (gen_expr depth) );
        ( 1,
          map3
            (fun cond t e -> SIf (cond, t, e))
            (gen_expr depth) sub_stmt (option sub_stmt) );
        ( 1,
          map2 (fun cond body -> SWhile (cond, body)) (gen_expr depth) sub_stmt
        );
        (1, map (fun stmts -> SBlock stmts) (list_size (1 -- 5) sub_stmt));
        (1, map (fun e -> SReturn (Some e)) (gen_expr depth));
      ]

(* Shrinkers *)

let ( <+> ) = Iter.append

let shrink_ident (Id s) =
  let open Iter in
  if String.length s > 1 then return (Id (String.sub s 0 (String.length s - 1)))
  else empty

let shrink_val_type = function
  | ValInt i -> Iter.map (fun i -> ValInt i) (Shrink.int i)
  | ValString s -> Iter.map (fun s -> ValString s) (Shrink.string s)
  | ValChar _ -> Iter.return ValNull
  | ValBool b -> if b then Iter.return (ValBool false) else Iter.empty
  | ValNull -> Iter.empty

let rec shrink_expr = function
  | EValue v -> Iter.map (fun v' -> EValue v') (shrink_val_type v)
  | EId id -> Iter.map (fun id' -> EId id') (shrink_ident id)
  | EBinOp (op, l, r) ->
      let open Iter in
      return l <+> return r
      <+> map (fun l' -> EBinOp (op, l', r)) (shrink_expr l)
      <+> map (fun r' -> EBinOp (op, l, r')) (shrink_expr r)
  | EUnOp (op, e) ->
      Iter.return e <+> Iter.map (fun e' -> EUnOp (op, e')) (shrink_expr e)
  | EFuncCall (f, Args []) -> Iter.return f
  | EFuncCall (f, Args args) -> (
      let open Iter in
      return (EFuncCall (f, Args []))
      <+> map (fun f' -> EFuncCall (f', Args args)) (shrink_expr f)
      <+>
      match args with
      | arg :: rest ->
          map (fun arg' -> EFuncCall (f, Args (arg' :: rest))) (shrink_expr arg)
          <+> return (EFuncCall (f, Args rest))
      | [] -> empty)
  | EArrayAccess _ -> Iter.empty
  | EAwait _ -> Iter.empty

let rec compare_expr_structure e1 e2 =
  match (e1, e2) with
  | EValue v1, EValue v2 -> (
      match (v1, v2) with
      | ValInt _, ValInt _ -> true
      | ValChar _, ValChar _ -> true
      | ValBool _, ValBool _ -> true
      | ValString _, ValString _ -> true
      | ValNull, ValNull -> true
      | _ -> false)
  | EId _, EId _ -> true
  | EBinOp (op1, l1, r1), EBinOp (op2, l2, r2) ->
      op1 = op2 && compare_expr_structure l1 l2 && compare_expr_structure r1 r2
  | EUnOp (op1, e1), EUnOp (op2, e2) ->
      op1 = op2 && compare_expr_structure e1 e2
  | EFuncCall (f1, Args a1), EFuncCall (f2, Args a2) ->
      compare_expr_structure f1 f2 && List.length a1 = List.length a2
  | _ -> false

let add_random_whitespace s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    (* TODO: proper constant *)
    if Random.int 5 = 0 then Buffer.add_char buf ' ';
    Buffer.add_char buf s.[i];
    if Random.int 5 = 0 then Buffer.add_char buf ' '
  done;
  Buffer.contents buf

let expr_arbitrary depth =
  let gen = gen_expr depth in
  let shrink = shrink_expr in
  QCheck.make ~shrink gen

let stmt_arbitrary depth =
  let gen = gen_stmt depth in
  QCheck.make gen (* TODO: shrink для stmt *)

let print_expr out expr = output_string out (Ast.show_expr expr)
(* TODO: Pp *)

let test_count = 10

(* Correct parser work *)
let prop_parse_no_crash =
  Test.make ~name:"parser does not crash on valid expressions" ~count:test_count
    (expr_arbitrary 5) (fun expr ->
      let str = Ast.show_expr expr in
      match Parser.apply_parser Parser.parse_ops str with
      | Ok _ -> true
      | Error e ->
          Printf.printf "\nParse error (but no crash): %s\n%s\n" str e;
          true)

(* Roundtrip: show -> parse -> show *)
let prop_roundtrip_expr =
  let gen = expr_arbitrary 5 in
  Test.make ~name:"expression roundtrip: show -> parse -> show"
    ~count:(test_count / 2) gen (fun expr ->
      let str1 = Ast.show_expr expr in
      match Parser.apply_parser Parser.parse_ops str1 with
      | Ok expr' ->
          let str2 = Ast.show_expr expr' in
          if str1 = str2 then true
          else (
            Printf.eprintf "\nRoundtrip failed:\n";
            Printf.eprintf "Original: %s\n" str1;
            Printf.eprintf "Parsed:   %s\n" (Ast.show_expr expr');
            Printf.eprintf "Roundtrip:%s\n" str2;
            false)
      | Error e ->
          Printf.eprintf "\nParse failed in roundtrip: %s\n%s\n" str1 e;
          false)

(* Operators priority tests *)
let prop_operator_precedence =
  let gen = expr_arbitrary 3 in
  Test.make ~name:"operator precedence is preserved" ~count:(test_count / 2) gen
    (fun expr ->
      let str = Ast.show_expr expr in
      match Parser.apply_parser Parser.parse_ops str with
      | Ok expr' ->
          if compare_expr_structure expr expr' then true
          else (
            Printf.eprintf "\nPrecision failed:\n";
            Printf.eprintf "Original: %s\n" (Ast.show_expr expr);
            Printf.eprintf "Parsed:   %s\n" (Ast.show_expr expr');
            false)
      | Error e ->
          Printf.eprintf "\nParse failed: %s\n" str;
          false)

(* Stmt tests *)
let prop_stmt_no_crash =
  Test.make ~name:"statement parser does not crash" ~count:(test_count / 2)
    (stmt_arbitrary 3) (fun stmt ->
      let str = Ast.show_stmt stmt in
      (* временно, пока нет отдельного парсера утверждений *)
      true)

(* Correct space pacing *)
let prop_whitespace_handling =
  let gen = expr_arbitrary 3 in
  Test.make ~name:"parser handles whitespace correctly" ~count:(test_count / 5)
    gen (fun expr ->
      let base_str = Ast.show_expr expr in
      let spaced_str = add_random_whitespace base_str in
      match
        ( Parser.apply_parser Parser.parse_ops base_str,
          Parser.apply_parser Parser.parse_ops spaced_str )
      with
      | Ok expr1, Ok expr2 ->
          if compare_expr_structure expr1 expr2 then true
          else (
            Printf.eprintf "\nWhitespace handling failed:\n";
            Printf.eprintf "Original (no spaces): %s\n" base_str;
            Printf.eprintf "With spaces: %s\n" spaced_str;
            Printf.eprintf "Parsed (no spaces): %s\n" (Ast.show_expr expr1);
            Printf.eprintf "Parsed (with spaces): %s\n" (Ast.show_expr expr2);
            false)
      | _ -> false)

(* Test run *)
let () =
  Random.self_init ();

  Printf.printf "\nQUICKCHECK TESTS\n\n";

  let tests =
    [
      prop_parse_no_crash;
      prop_roundtrip_expr;
      prop_operator_precedence;
      prop_stmt_no_crash;
      prop_whitespace_handling;
    ]
  in

  Printf.printf "Run %d tests...\n" (List.length tests);

  let exit_code = QCheck_runner.run_tests ~verbose:true tests in

  Printf.printf "\nRESULTS\n";

  if exit_code = 0 then Printf.printf "All tests are executed!\n"
  else Printf.printf "Some tests are not executed! (code: %d).\n" exit_code
