[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  | Parse_error of string
  | Unbound_variable of string
  | Division_by_zero
  | Not_a_function
  | Type_error of string
  | Steps_exceeded

let format_error = function
  | Parse_error _ -> "Parse error"
  | Unbound_variable x -> "Error: unbound variable " ^ x
  | Division_by_zero -> "Error: division by zero"
  | Not_a_function -> "Error: not a function"
  | Type_error msg -> "Error: " ^ msg
  | Steps_exceeded -> "Error: step limit exceeded"
;;

type state =
  { steps : int (** remaining evaluation steps *)
  ; output : string list (** accumulated output lines in reverse order *)
  }

type 'a t = state -> ('a * state, error) result

let return (x : 'a) : 'a t = fun st -> Ok (x, st)

let ( let* ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
  fun st ->
  match m st with
  | Error e -> Error e
  | Ok (x, st') -> f x st'
;;

let fail (e : error) : 'a t = fun _st -> Error e

let tick : unit t =
  fun st ->
  if st.steps <= 0 then Error Steps_exceeded else Ok ((), { st with steps = st.steps - 1 })
;;

let emit (line : string) : unit t =
  fun st -> Ok ((), { st with output = line :: st.output })
;;

let lift (r : ('a, error) result) : 'a t =
  fun st ->
  match r with
  | Ok x -> Ok (x, st)
  | Error e -> Error e
;;

type value =
  | VInt of int
  | VClosure of id * expr * env
  | VBuiltin of (value -> value t)

and env = (id * value) list

let pp_value fmt = function
  | VInt n -> Format.fprintf fmt "%d" n
  | VClosure _ -> Format.fprintf fmt "<closure>"
  | VBuiltin _ -> Format.fprintf fmt "<builtin>"
;;

let format_value = function
  | VInt n -> string_of_int n
  | VClosure _ | VBuiltin _ -> "<fun>"
;;

let run_eval (steps : int) (m : 'a t) : ('a * string list, error) result =
  match m { steps; output = [] } with
  | Ok (x, st) -> Ok (x, List.rev st.output)
  | Error e -> Error e
;;

let eval_binop op a b : (int, error) result =
  let bool i = if i then 1 else 0 in
  match op with
  | Add -> Ok (a + b)
  | Sub -> Ok (a - b)
  | Mul -> Ok (a * b)
  | Div -> if b = 0 then Error Division_by_zero else Ok (a / b)
  | Eq -> Ok (bool (a = b))
  | Neq -> Ok (bool (a <> b))
  | Lt -> Ok (bool (a < b))
  | Le -> Ok (bool (a <= b))
  | Gt -> Ok (bool (a > b))
  | Ge -> Ok (bool (a >= b))
;;

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> return v
  | None -> fail (Unbound_variable x)
;;

let require_int = function
  | VInt n -> return n
  | _ -> fail (Type_error "expected an integer")
;;

let print_builtin v =
  let* n = require_int v in
  let* () = emit (string_of_int n) in
  return (VInt 0)
;;

let initial_env : env = [ "print", VBuiltin print_builtin ]

let rec eval env expr =
  let* () = tick in
  match expr with
  | EConst n -> return (VInt n)
  | EVar x -> lookup env x
  | EFun (x, body) -> return (VClosure (x, body, env))
  | EBinop (op, e1, e2) ->
    let* v1 = eval env e1 in
    let* a = require_int v1 in
    let* v2 = eval env e2 in
    let* b = require_int v2 in
    let* n = lift (eval_binop op a b) in
    return (VInt n)
  | EIf (cond, e1, e2) ->
    let* vc = eval env cond in
    let* n = require_int vc in
    if n <> 0 then eval env e1 else eval env e2
  | EApp (f_expr, arg_expr) ->
    let* fv = eval env f_expr in
    let* argv = eval env arg_expr in
    (match fv with
     | VClosure (x, body, clo_env) -> eval ((x, argv) :: clo_env) body
     | VBuiltin f -> f argv
     | VInt _ -> fail Not_a_function)
  | ELet (NonRec, x, e1, e2) ->
    let* v1 = eval env e1 in
    eval ((x, v1) :: env) e2
  | ELet (Rec, f, e1, e2) ->
    (match e1 with
     | EFun (x, body) ->
       let rec env' = (f, VClosure (x, body, env')) :: env in
       eval env' e2
     | _ -> fail (Type_error "let rec requires a function on the right-hand side"))
;;

let run ?(steps = 10_000) (src : string) : (value * string list, error) result =
  match Parser.parse src with
  | Error msg -> Error (Parse_error msg)
  | Ok ast -> run_eval steps (eval initial_env ast)
;;
