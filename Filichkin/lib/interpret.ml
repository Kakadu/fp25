[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

type error =
  | UnboundVariable of string
  | DivisionByZero
  | TypeError of string
  | UnsupportedConstruct of string
  | IncorrectExpression
  | StepCountIsZero

type 'a eval_result = ('a, error) Result.t

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
;;

type env = (string * value) list

and value =
  | VInt of int
  | VUnit
  | VBool of bool
  | VTuple of value list
  | VClosure of pattern * expr * env
  | VBuiltin of (value -> value eval_result)
  | VConstr of string * value list

let rec look_up x = function
  | [] -> None
  | (y, v) :: _ when String.equal x y -> Some v
  | _ :: tl -> look_up x tl
;;

let return x = Ok x
let err e = Error e

let eval_binop op v1 v2 =
  match op, v1, v2 with
  | Plus, VInt a, VInt b -> return (VInt (a + b))
  | Minus, VInt a, VInt b -> return (VInt (a - b))
  | Mult, VInt a, VInt b -> return (VInt (a * b))
  | Div, VInt _, VInt 0 -> err DivisionByZero
  | Div, VInt a, VInt b -> return (VInt (a / b))
  | Equal, VInt a, VInt b -> return (VBool (a = b))
  | NotEqual, VInt a, VInt b -> return (VBool (a <> b))
  | More, VInt a, VInt b -> return (VBool (a > b))
  | Less, VInt a, VInt b -> return (VBool (a < b))
  | EMore, VInt a, VInt b -> return (VBool (a >= b))
  | ELess, VInt a, VInt b -> return (VBool (a <= b))
  | And, VBool a, VBool b -> return (VBool (a && b))
  | Or, VBool a, VBool b -> return (VBool (a || b))
  | And, _, _ | Or, _, _ -> err (TypeError "&& and || expect booleans")
  | (Equal | NotEqual | More | Less | EMore | ELess), _, _ ->
    err (TypeError "comparisons expect integers")
  | _ -> err (TypeError "binary operation expected two integers")
;;

let rec match_pattern (pat : pattern) (v : value) : (string * value) list option =
  match pat, v with
  | PVar x, _ -> Some [ x, v ]
  | PTuple ps, VTuple vs ->
    if List.length ps <> List.length vs
    then None
    else (
      try
        let bindings =
          List.map2_exn ps vs ~f:(fun p v ->
            match match_pattern p v with
            | Some b -> b
            | None -> failwith "match_fail")
        in
        Some (List.concat bindings)
      with
      | _ -> None)
  | PConstr (name, ps), VConstr (vname, vs) ->
    if String.equal name vname && List.length ps = List.length vs
    then (
      let rec collect acc ps vs =
        match ps, vs with
        | [], [] -> Some acc
        | p :: ps', v :: vs' ->
          (match match_pattern p v with
           | Some bs -> collect (acc @ bs) ps' vs'
           | None -> None)
        | _ -> None
      in
      collect [] ps vs)
    else None
  | _ -> None
;;

(** Main interpretation function *)
let rec eval (env : env) (e : expr) (steps : int) : value eval_result =
  if steps < 0
  then err StepCountIsZero
  else (
    let steps = steps - 1 in
    match e with
    | Int n -> return (VInt n)
    | Var x ->
      (match look_up x env with
       | Some v -> return v
       | None ->
         if Char.is_uppercase x.[0]
         then return (VConstr (x, []))
         else err (UnboundVariable x))
    | Bool b -> return (VBool b)
    | Tuple es ->
      let rec eval_elements acc = function
        | [] -> return (VTuple (List.rev acc))
        | head :: tail ->
          let* v = eval env head steps in
          eval_elements (v :: acc) tail
      in
      eval_elements [] es
    | BinOp (op, e1, e2) ->
      let* v1 = eval env e1 steps in
      let* v2 = eval env e2 steps in
      eval_binop op v1 v2
    | If (cond, then_e, else_e) ->
      let* v = eval env cond steps in
      (match v with
       | VBool false -> eval env else_e steps
       | VBool true -> eval env then_e steps
       | _ -> err (TypeError "if condition must be boolean"))
    | Let (flag, pat, bound, body_opt) ->
      (match flag with
       | NonRec ->
         let* v_bound = eval env bound steps in
         (match match_pattern pat v_bound with
          | Some bindings ->
            let new_env = bindings @ env in
            (match body_opt with
             | Some body -> eval new_env body steps
             | None -> return VUnit)
          | None -> err (TypeError "Pattern match failed in let-binding"))
       | Rec ->
         (match pat with
          | PVar name ->
            (match bound with
             | Abs (pat', fun_body) ->
               let rec closure = VClosure (pat', fun_body, rec_env)
               and rec_env = (name, closure) :: env in
               (match body_opt with
                | Some body -> eval rec_env body steps
                | None -> return VUnit)
             | _ -> err (TypeError "recursive binding must be a function"))
          | PTuple _ -> err (TypeError "Recursive let-tuple is not supported")))
    | Abs (pat, body) -> return (VClosure (pat, body, env))
    | Match (scrutinee, cases) ->
      let* v = eval env scrutinee steps in
      let rec try_cases = function
        | [] -> err (TypeError "non-exhaustive pattern matching")
        | (pat, expr) :: rest ->
          (match match_pattern pat v with
           | Some bindings ->
             let new_env = bindings @ env in
             eval new_env expr steps
           | None -> try_cases rest)
      in
      try_cases cases
    | UnOp (op, e1) ->
      let* v1 = eval env e1 steps in
      (match op, v1 with
       | Neg, VInt n -> return (VInt (-n))
       | Not, VBool n -> return (VBool (not n))
       | Neg, _ -> err (TypeError "negation expects integer")
       | Not, _ -> err (TypeError "not expects boolean"))
    | App (f, arg) ->
      let* vf = eval env f steps in
      let* va = eval env arg steps in
      (match vf with
       | VConstr (name, args) -> return (VConstr (name, args @ [ va ]))
       | VClosure (pat, body, closure_env) ->
         (match match_pattern pat va with
          | Some bindings ->
            let new_env = bindings @ closure_env in
            eval new_env body steps
          | None -> err (TypeError "Pattern match failed in function application"))
       | VBuiltin f -> f va
       | _ -> err (TypeError "application expects a function")))
;;

let init_env =
  let print_int_fun =
    VBuiltin
      (function
        | VInt x ->
          Stdlib.Printf.printf "%d\n" x;
          return VUnit
        | _ -> err (TypeError "print_int expects an integer"))
  in
  let print_bool_fun =
    VBuiltin
      (function
        | VBool b ->
          Stdlib.Printf.printf "%b\n" b;
          return VUnit
        | _ -> err (TypeError "print_bool expects a boolean"))
  in
  [ "print_int", print_int_fun
  ; "true", VBool true
  ; "false", VBool false
  ; "print_bool", print_bool_fun
  ]
;;

let run_interpret expr = eval init_env expr 1000

let rec string_of_value = function
  | VInt n -> Int.to_string n
  | VBool b -> Bool.to_string b
  | VUnit -> "()"
  | VTuple vs -> "(" ^ (List.map vs ~f:string_of_value |> String.concat ~sep:", ") ^ ")"
  | VClosure _ -> "<fun>"
  | VBuiltin _ -> "<builtin>"
  | VClosure _ -> "<fun>"
  | VConstr (name, _) -> name
;;

let string_of_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | DivisionByZero -> "Division by zero"
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | UnsupportedConstruct constr -> Printf.sprintf "Unsupported construct: %s" constr
  | IncorrectExpression -> "Incorrect expression"
  | StepCountIsZero -> "Step count is zero"
;;
