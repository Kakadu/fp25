[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let rec pattern = function
  | PVar x -> x
  | PTuple ps ->
    let ps_str = List.map pattern ps |> String.concat ", " in
    String.concat "" [ "("; ps_str; ")" ]
  | PConstr (name, args) ->
    (match args with
     | [] -> name
     | args ->
       let args_str = List.map pattern args |> String.concat ", " in
       String.concat "" [ name; " ("; args_str; ")" ])
  | PWildcard -> "_"
;;

let rec pattern_ast = function
  | PVar x -> Printf.sprintf "Var %s" x
  | PTuple ps ->
    let ps_str = List.map pattern_ast ps |> String.concat "; " in
    Printf.sprintf "PTuple [%s]" ps_str
  | PConstr (name, args) ->
    let ps_str = List.map pattern_ast args |> String.concat "; " in
    Printf.sprintf "PConstr %s, [%s]" name ps_str
  | PWildcard -> "_"
;;

let rec string_of_type_expr = function
  | TEInt -> "int"
  | TEBool -> "bool"
  | TEUnit -> "unit"
  | TEVar v -> String.concat "" [ "'"; v ]
  | TEArrow (t1, t2) ->
    Printf.sprintf "(%s -> %s)" (string_of_type_expr t1) (string_of_type_expr t2)
  | TETuple ts ->
    let ts_str = List.map string_of_type_expr ts |> String.concat " * " in
    Printf.sprintf "(%s)" ts_str
  | TEConstr (name, args) ->
    (match args with
     | [] -> name
     | _ ->
       let args_str = List.map string_of_type_expr args |> String.concat " " in
       Printf.sprintf "%s %s" name args_str)
;;

let rec type_expr_to_ast = function
  | TEInt -> "TEInt"
  | TEBool -> "TEBool"
  | TEUnit -> "TEUnit"
  | TEVar v -> Printf.sprintf "TEVar %s" v
  | TEArrow (t1, t2) ->
    Printf.sprintf "TEArrow (%s, %s)" (type_expr_to_ast t1) (type_expr_to_ast t2)
  | TETuple ts ->
    let ts_str = List.map type_expr_to_ast ts |> String.concat "; " in
    Printf.sprintf "TETuple [%s]" ts_str
  | TEConstr (name, args) ->
    let args_str = List.map type_expr_to_ast args |> String.concat "; " in
    Printf.sprintf "TEConstr (%s, [%s])" name args_str
;;

let rec print_ast = function
  | Int n -> Printf.sprintf "Int %d" n
  | Var x -> Printf.sprintf "Var %S" x
  | Constr name -> Printf.sprintf "Constr %S" name
  | BinOp (op, l, r) ->
    let op_str =
      match op with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Mult -> "Mult"
      | Div -> "Div"
      | Equal -> "Equal"
      | More -> "More"
      | Less -> "Less"
      | ELess -> "ELess"
      | EMore -> "EMore"
      | NotEqual -> "NotEqual"
      | And -> "And"
      | Or -> "Or"
    in
    Printf.sprintf "BinOp (%s, %s, %s)" op_str (print_ast l) (print_ast r)
  | If (c, t, e) ->
    Printf.sprintf "If (%s, %s, %s)" (print_ast c) (print_ast t) (print_ast e)
  | Let (NonRec, x, e, None) ->
    Printf.sprintf "Let (NonRec, %S, %s, None)" (pattern_ast x) (print_ast e)
  | Let (NonRec, x, e, Some b) ->
    Printf.sprintf
      "Let (NonRec, %S, %s, Some %s)"
      (pattern_ast x)
      (print_ast e)
      (print_ast b)
  | Let (Rec, x, e, None) ->
    Printf.sprintf "Let (Rec, %S, %s, None)" (pattern_ast x) (print_ast e)
  | Let (Rec, x, e, Some b) ->
    Printf.sprintf
      "Let (Rec, %S, %s, Some %s)"
      (pattern_ast x)
      (print_ast e)
      (print_ast b)
  | Abs (param, body) -> Printf.sprintf "Abs (%S, %s)" (pattern param) (print_ast body)
  | App (f, a) -> Printf.sprintf "App (%s, %s)" (print_ast f) (print_ast a)
  | UnOp (op, e) ->
    let op_str =
      match op with
      | Neg -> "Neg"
      | Not -> "Not"
    in
    Printf.sprintf "UnOp (%s, %s)" op_str (print_ast e)
  | Bool b -> Printf.sprintf "Bool (%b)" b
  | Tuple es ->
    let es_str = List.map print_ast es |> String.concat "; " in
    Printf.sprintf "Tuple [%s]" es_str
  | Match (scrutinee, cases) ->
    let cases_str =
      List.map
        (fun (p, e) -> Printf.sprintf "(%s, %s)" (pattern_ast p) (print_ast e))
        cases
      |> String.concat "; "
    in
    Printf.sprintf "Match (%s, [%s])" (print_ast scrutinee) cases_str
;;

let rec print_expr = function
  | Int n -> string_of_int n
  | Var s -> s
  | Constr name -> name
  | BinOp (op, left, right) ->
    let oper =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Equal -> "="
      | More -> ">"
      | Less -> "<"
      | EMore -> ">="
      | ELess -> "<="
      | NotEqual -> "<>"
      | And -> "&&"
      | Or -> "||"
    in
    let left_str = print_expr left in
    let right_str = print_expr right in
    Printf.sprintf "((%s) %s (%s))" left_str oper right_str
  | UnOp (op, e) ->
    let oper =
      match op with
      | Neg -> "-"
      | Not -> "not"
    in
    Printf.sprintf "(%s (%s))" oper (print_expr e)
  | If (cond, thn, els) ->
    let cond_str = print_expr cond in
    let thn_str = print_expr thn in
    let els_str = Printf.sprintf " else %s" (print_expr els) in
    Printf.sprintf "(if %s then %s%s)" cond_str thn_str els_str
  | Let (rec_f, name, value, body) ->
    let rec_prefix =
      match rec_f with
      | Rec -> "rec "
      | NonRec -> ""
    in
    let value_str = print_expr value in
    let body_str =
      match body with
      | None -> ""
      | Some b -> Printf.sprintf " in %s" (print_expr b)
    in
    Printf.sprintf "(let %s%s = %s%s)" rec_prefix (pattern name) value_str body_str
  | Abs (param, body) -> Printf.sprintf "(fun %s -> %s)" (pattern param) (print_expr body)
  | App (func, arg) -> Printf.sprintf "(%s %s)" (print_expr func) (print_expr arg)
  | Bool b -> Printf.sprintf "%b" b
  | Tuple es ->
    let es_str = List.map print_expr es |> String.concat ", " in
    Printf.sprintf "(%s)" es_str
  | Match (scrutinee, cases) ->
    let cases_str =
      List.map
        (fun (p, e) -> Printf.sprintf "| %s -> %s" (pattern p) (print_expr e))
        cases
      |> String.concat " "
    in
    Printf.sprintf "(match %s with %s)" (print_expr scrutinee) cases_str
;;

let type_decl_to_ast td =
  let constrs_str =
    List.map
      (fun c ->
        let args_str = List.map type_expr_to_ast c.ctor_args |> String.concat "; " in
        Printf.sprintf "{ ctor_name = %s; ctor_args = [%s] }" c.ctor_name args_str)
      td.constructors
    |> String.concat "; "
  in
  Printf.sprintf
    "[type_name %s, type_params [%s], constructors [%s]]"
    td.type_name
    (String.concat "; " td.type_params)
    constrs_str
;;

let type_decl_to_string td =
  let params_str =
    match td.type_params with
    | [] -> ""
    | ps -> String.concat "" [ "'"; String.concat " '" ps; " " ]
  in
  let constrs_str =
    List.map
      (fun c ->
        match c.ctor_args with
        | [] -> c.ctor_name
        | args ->
          let args_str = List.map string_of_type_expr args |> String.concat " * " in
          Printf.sprintf "%s of %s" c.ctor_name args_str)
      td.constructors
    |> String.concat " | "
  in
  Printf.sprintf "type %s%s = %s" params_str td.type_name constrs_str
;;

let toplevel_to_ast = function
  | TLExpr e -> print_ast e
  | TLType td -> type_decl_to_ast td
;;

let toplevel_to_string = function
  | TLExpr e -> print_expr e
  | TLType td -> type_decl_to_string td
;;

let print_ast_p tpl = List.iter (fun tl -> Printf.printf "%s\n" (toplevel_to_ast tl)) tpl
let print_p tpl = List.iter (fun tl -> Printf.printf "%s\n" (toplevel_to_string tl)) tpl
