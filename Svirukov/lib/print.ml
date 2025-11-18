open Ast
open Interpret

let rec print_ast = function
  | Constant CUnit -> "CUnit"
  | Constant (CInt n) -> Printf.sprintf "CInt(%d)" n
  | Var pat ->
    let line =
      match pat with
      | PAny -> "Var(_)"
      | PVar name -> Printf.sprintf "Var(%s)" name
    in
    line
  | Binop (op, left, right) ->
    let l = print_ast left in
    let r = print_ast right in
    let s =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Asteriks -> "*"
      | Dash -> "/"
      | Equals -> "="
      | MoreThan -> ">"
      | LessThan -> "<"
      | EqLess -> "<="
      | EqMore -> ">="
    in
    Printf.sprintf "(%s %s %s)" l s r
  | Conditional (cond, main, alt) ->
    let cond = print_ast cond in
    let main = print_ast main in
    let res =
      match alt with
      | Some ex -> Printf.sprintf "if (%s) then (%s) else (%s)" cond main (print_ast ex)
      | None -> Printf.sprintf "if (%s) then (%s)" cond main
    in
    res
  | Let (flag, pat, body, next) ->
    let b = print_ast body in
    let res =
      match next, flag, pat with
      | Some next, Rec, PVar name ->
        let cont = print_ast next in
        Printf.sprintf "(let rec %s = %s in %s)" name b cont
      | None, NonRec, PVar name -> Printf.sprintf "(let %s = %s)" name b
      | Some next, NonRec, PVar name ->
        let cont = print_ast next in
        Printf.sprintf "(let %s = %s in %s)" name b cont
      | Some next, Rec, PAny ->
        let cont = print_ast next in
        Printf.sprintf "(let rec _ = %s in %s)" b cont
      | None, NonRec, PAny -> Printf.sprintf "(let _ = %s)" b
      | Some next, NonRec, PAny ->
        let cont = print_ast next in
        Printf.sprintf "(let _ = %s in %s)" b cont
      | None, Rec, PVar name -> Printf.sprintf "(let rec %s = %s)" name b
      | None, Rec, _ -> Printf.sprintf "(let rec _ = %s)" b
    in
    res
  | Fun (var, body) ->
    Printf.sprintf "Fun (%s, %s)" (print_ast (Var var)) (print_ast body)
  | App (left, right) -> Printf.sprintf "App (%s, %s)" (print_ast left) (print_ast right)
;;

let print_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable %s" name
  | TypeError err -> err
  | DivisionByZero -> "Division by zero"
  | Unimplemented -> "Not implemented yet..."
;;
