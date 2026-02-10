[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type prim =
  | Print_int
  | Fix

type value =
  | VInt of int
  | VClosure of closure
  | VPrim of prim
  | VUnit

and closure =
  { param : name
  ; body : expr
  ; env : env
  }

and env = (name * value) list

type error =
  | Unknown_variable of name
  | Not_a_function of value
  | Division_by_zero
  | Type_error of string
  | Out_of_fuel

type 'a eval_result = ('a, error) result

let ok x = Ok x
let error e = Error e

let ( let* ) (m : 'a eval_result) (f : 'a -> 'b eval_result) : 'b eval_result =
  match m with
  | Ok v -> f v
  | Error e -> Error e
;;

type fuel = int

let tick (fuel : fuel) : (unit * fuel, error) result =
  if fuel <= 0 then error Out_of_fuel else ok ((), fuel - 1)
;;

let lookup (env : env) (x : name) : value option = List.assoc_opt x env

let string_of_value = function
  | VInt n -> string_of_int n
  | VUnit -> "()"
  | VClosure _ -> "<fun>"
  | VPrim Print_int -> "<prim print_int>"
  | VPrim Fix -> "<prim fix>"
;;

let string_of_error : error -> string = function
  | Unknown_variable x -> "Unknown variable: " ^ x
  | Not_a_function v -> "Not a function: " ^ string_of_value v
  | Division_by_zero -> "Division by zero"
  | Type_error msg -> "Type error: " ^ msg
  | Out_of_fuel -> "Out of fuel"
;;

let rec apply (fuel : fuel) (f : value) (arg : value) : (value * fuel, error) result =
  match f with
  | VClosure { param; body; env } ->
    let env' = (param, arg) :: env in
    eval env' fuel body
  | VPrim Print_int ->
    (match arg with
     | VInt n ->
       print_endline (string_of_int n);
       ok (VUnit, fuel)
     | v -> error (Type_error ("print_int expects int, got " ^ string_of_value v)))
  | VPrim Fix ->
    (match arg with
     | VClosure { param = self; body; env } ->
       (match body with
        | Abs (arg, inner_body) ->
          let rec rec_closure =
            VClosure { param = arg; body = inner_body; env = (self, rec_closure) :: env }
          in
          ok (rec_closure, fuel)
        | _ -> error (Type_error "fix expects a function that returns a function"))
     | v -> error (Type_error ("fix expects a function, got " ^ string_of_value v)))
  | v -> error (Not_a_function v)

and eval (env : env) (fuel : fuel) (e : expr) : (value * fuel, error) result =
  let* (), fuel = tick fuel in
  match e with
  | Var x ->
    (match lookup env x with
     | Some v -> ok (v, fuel)
     | None -> error (Unknown_variable x))
  | Int n -> ok (VInt n, fuel)
  | Abs (x, body) -> ok (VClosure { param = x; body; env }, fuel)
  | App (e1, e2) ->
    let* f, fuel1 = eval env fuel e1 in
    let* arg, fuel2 = eval env fuel1 e2 in
    apply fuel2 f arg
  | Let (x, e1, e2) ->
    let* v1, fuel1 = eval env fuel e1 in
    let env' = (x, v1) :: env in
    eval env' fuel1 e2
  | Let_rec (f, rhs, body) ->
    (match rhs with
     | Abs (x, fun_body) ->
       let rec closure = VClosure { param = x; body = fun_body; env = env' }
       and env' = (f, closure) :: env in
       eval env' fuel body
     | _ -> error (Type_error "let rec expects a function on the right-hand side"))
  | If (cond, e_then, e_else) ->
    let* v_cond, fuel1 = eval env fuel cond in
    (match v_cond with
     | VInt n -> if n <> 0 then eval env fuel1 e_then else eval env fuel1 e_else
     | _ -> error (Type_error "if condition must be an int"))
  | Binop (op, e1, e2) ->
    let* v1, fuel1 = eval env fuel e1 in
    let* v2, fuel2 = eval env fuel1 e2 in
    (match v1, v2 with
     | VInt n1, VInt n2 ->
       let* n =
         match op with
         | Add -> ok (n1 + n2)
         | Sub -> ok (n1 - n2)
         | Mul -> ok (n1 * n2)
         | Div -> if n2 = 0 then error Division_by_zero else ok (n1 / n2)
       in
       ok (VInt n, fuel2)
     | _ -> error (Type_error "integer operands expected in arithmetic"))
  | Cmp (op, e1, e2) ->
    let* v1, fuel1 = eval env fuel e1 in
    let* v2, fuel2 = eval env fuel1 e2 in
    (match v1, v2 with
     | VInt n1, VInt n2 ->
       let b =
         match op with
         | Eq -> n1 = n2
         | Neq -> n1 <> n2
         | Lt -> n1 < n2
         | Le -> n1 <= n2
         | Gt -> n1 > n2
         | Ge -> n1 >= n2
       in
       ok (VInt (if b then 1 else 0), fuel2)
     | _ -> error (Type_error "comparison expects integer operands"))
;;

let initial_env : env = [ "print_int", VPrim Print_int; "fix", VPrim Fix ]

type run_error =
  | RuntimeError of error
  | ParseError of Parser.error

let string_of_run_error (err : run_error) : string =
  match err with
  | RuntimeError e -> string_of_error e
  | ParseError e -> Format.asprintf "%a" Parser.pp_error e
;;

let run_program ?(fuel = 100_000) (str : string) : (value * fuel, run_error) result =
  match Parser.parse str with
  | Result.Error e -> Error (ParseError e)
  | Result.Ok ast ->
    (match eval initial_env fuel ast with
     | Ok v -> Ok v
     | Error e -> Error (RuntimeError e))
;;

let parse_and_run ?fuel (str : string) : unit =
  match run_program ?fuel str with
  | Ok (v, _fuel_left) -> Format.printf "Success: %s\n%!" (string_of_value v)
  | Error err -> Format.eprintf "Error: %s\n%!" (string_of_run_error err)
;;
