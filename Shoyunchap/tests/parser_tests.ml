(* test_parser.ml *)
open Lambda_lib

let test_addition () =
  match Parser.parse "1 + 2" with
  | Ok (Ast.BinOp (Ast.OpAdd, Ast.Const (Ast.Int 1), Ast.Const (Ast.Int 2))) -> ()
  | _ -> failwith "failed to parse simple addition"

let test_let_binding () =
  match Parser.parse "let x = 1 in x" with
  | Ok (Ast.Let (_, Ast.NonRec, "x", Ast.Const (Ast.Int 1), Some (Ast.Var "x"))) -> ()
  | _ -> failwith "failed to parse let binding"

let () =
  test_addition ();
  test_let_binding ()
