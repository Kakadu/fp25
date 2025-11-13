open Ast

let rec printer = function
  | Constant (CInt n) -> Printf.sprintf "%d" n
  | Var pat ->
    let line =
      match pat with
      | PAny -> "_"
      | PVar name -> Printf.sprintf "%s" name
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
      | None -> Printf.sprintf "if (%s) then (%s)" cond main
      | Some op ->
        let alt = printer op in
        Printf.sprintf "if (%s) then (%s) else (%s)" cond main alt
    in
    res
  | Let (flag, PVar name, body, next) ->
    let b = printer body in
    let res =
      match next, flag with
      | Some next, Rec ->
        let cont = printer next in
        Printf.sprintf "(let rec %s = %s in %s)" name b cont
      | None, NonRec -> Printf.sprintf "(let rec %s = %s)" name b
      | Some next, NonRec ->
        let cont = printer next in
        Printf.sprintf "(let %s = %s in %s)" name b cont
      | None, Rec -> raise (Invalid_argument "No sense creating rec func without name")
    in
    res
  | _ -> "Can not parse them yet"
;;
