(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Interpret

let rec print_ast = function
  | Constant CUnit -> "()"
  | Constant (CInt n) -> string_of_int n
  | Var (PVar name) -> name
  | Binop (op, left, right) ->
    let op_str =
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
    Printf.sprintf "(%s %s %s)" (print_ast left) op_str (print_ast right)
  | Conditional (cond, main, None) ->
    Printf.sprintf "(if %s then %s)" (print_ast cond) (print_ast main)
  | Conditional (cond, main, Some alt) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (print_ast cond)
      (print_ast main)
      (print_ast alt)
  | Let (NonRec, PVar name, body, None) ->
    Printf.sprintf "(let %s = %s)" name (print_ast body)
  | Let (NonRec, PVar name, body, Some next) ->
    Printf.sprintf "(let %s = %s in %s)" name (print_ast body) (print_ast next)
  | Let (Rec, PVar name, body, None) ->
    Printf.sprintf "(let rec %s = %s)" name (print_ast body)
  | Let (Rec, PVar name, body, Some next) ->
    Printf.sprintf "(let rec %s = %s in %s)" name (print_ast body) (print_ast next)
  | Func (PVar arg, body) -> Printf.sprintf "(fun %s -> %s)" arg (print_ast body)
  | App (left, right) -> Printf.sprintf "%s %s" (print_ast left) (print_ast right)
;;

let print_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable %s" name
  | TypeError err -> err
  | DivisionByZero -> "Division by zero"
  | Unimplemented -> "Not implemented yet..."
  | TooManyArgs -> "Too many args for function"
  | ParttialApplication -> "Not enought args to calculate function"
  | ExceedNumberOfSteps expr ->
    Printf.sprintf "Exceed number of redunction posssible: \n%s" (print_ast expr)
;;
