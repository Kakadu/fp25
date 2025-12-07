(*open Ast*)

let rec pprint ast =
  match ast with
  | Ast.Const x -> string_of_int x
  | Ast.Ident x -> x
  | Ast.Binexpr (op, l, r) ->
    let op_str =
      match op with
      | Ast.Plus -> "+"
      | Ast.Minus -> "-"
      | Ast.Mul -> "*"
      | Ast.Div -> "/"
      | Ast.Eq -> "="
      | Ast.Bi -> ">"
      | Ast.Neq -> "!="
      | Ast.Le -> "<"
      | _ -> "?"
    in
    "(" ^ pprint l ^ ") " ^ op_str ^ " (" ^ pprint r ^ ")"
  | Ast.Ite (cond, tb, eb) ->
    "if " ^ pprint cond ^ " then " ^ pprint tb ^ " else " ^ pprint eb
  | Ast.Let (Ast.Recflag b, Ast.Ident name, letexpr, inexpr) ->
    let rec_flag = if b then "rec " else "" in
    "let " ^ rec_flag ^ name ^ " = " ^ pprint letexpr ^ " in " ^ pprint inexpr
  | Ast.Abs (Ast.Ident arg, f) -> "fun " ^ arg ^ " -> " ^ pprint f
  | Ast.App (f, arg) -> "(" ^ pprint f ^ ") (" ^ pprint arg ^ ")"
  | _ -> "bad ast"
;;
