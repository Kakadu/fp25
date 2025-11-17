open Ast

let rec printer = function
  | Constant (CInt n) -> Printf.sprintf "%d" n
  | Var pat ->
    let line =
      match pat with
      | PAny -> "_"
      | PVar name -> Printf.sprintf "Var(%s)" name
    in
    line
  | Binop (op, left, right) ->
    let l = printer left in
    let r = printer right in
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
    let cond = printer cond in
    let main = printer main in
    let res =
      match alt with
      | Some ex -> Printf.sprintf "if (%s) then (%s) else (%s)" cond main (printer ex)
      | None -> Printf.sprintf "if (%s) then (%s)" cond main
    in
    res
  | Let (flag, pat, body, next) ->
    let b = printer body in
    let res =
      match next, flag, pat with
      | Some next, Rec, PVar name ->
        let cont = printer next in
        Printf.sprintf "(let rec %s = %s in %s)" name b cont
      | None, NonRec, PVar name -> Printf.sprintf "(let %s = %s)" name b
      | Some next, NonRec, PVar name ->
        let cont = printer next in
        Printf.sprintf "(let %s = %s in %s)" name b cont
      | Some next, Rec, PAny ->
        let cont = printer next in
        Printf.sprintf "(let rec _ = %s in %s)" b cont
      | None, NonRec, PAny -> Printf.sprintf "(let _ = %s)" b
      | Some next, NonRec, PAny ->
        let cont = printer next in
        Printf.sprintf "(let _ = %s in %s)" b cont
      | None, Rec, PVar name -> Printf.sprintf "(let rec %s = %s)" name b
      | None, Rec, _ -> raise (Invalid_argument "No sense creating rec func without name")
    in
    res
  | Fun (var, body) -> Printf.sprintf "Fun (%s, %s)" (printer (Var var)) (printer body)
  | App (left, right) -> Printf.sprintf "App (%s, %s)" (printer left) (printer right)
  | _ -> "Can not parse them yet"
;;
