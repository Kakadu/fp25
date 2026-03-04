(** Copyright 2026, [ChurinNick] *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast



type error =
  | UnboundVariable of var
  | NotAnInteger of value
  | NotAFunction of value
  | DivisionByZero
  | StepLimitExceeded
  | InvalidCondition of value
  | FixAppliedToNonFunction

and value =
  | VInt of int
  | VUnit
  | VClosure of var * expr * env  
  | VRecClosure of var * var * expr * env  
  | VBuiltin of (value -> (value, error) result)

and env = (var * value) list



module InterpM : sig
  type 'a t

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : error -> 'a t
  val tick : unit t
  val run : 'a t -> int -> ('a, error) result
end = struct
  type 'a t = int -> int * ('a, error) result

  let return x steps = (steps, Ok x)

  let ( let* ) m f steps =
    let steps', res = m steps in
    match res with
    | Ok x -> f x steps'
    | Error e -> (steps', Error e)

  let ( >>= ) = ( let* )

  let fail e steps = (steps, Error e)

  let tick steps =
    if steps <= 0 then (steps, Error StepLimitExceeded)
    else (steps - 1, Ok ())

  let run m steps = snd (m steps)
end



open InterpM

let empty_env : env = []

let extend_env env name value = (name, value) :: env

let rec lookup_env env name =
  match env with
  | [] -> fail (UnboundVariable name)
  | (key, value) :: rest -> if key = name then return value else lookup_env rest name


let builtin_print =
  VBuiltin (fun v ->
    match v with
    | VInt n ->
        print_int n;
        print_newline ();
        Ok VUnit
    | _ -> Error (NotAnInteger v))

let initial_env : env =
  let env = extend_env empty_env "print_int" builtin_print in
  extend_env env "print" builtin_print



let expect_int = function
  | VInt n -> return n
  | v -> fail (NotAnInteger v)

let expect_function = function
  | VClosure _ as f -> return f
  | VRecClosure _ as f -> return f
  | VBuiltin _ as f -> return f
  | v -> fail (NotAFunction v)

let is_truthy = function
  | VInt 0 -> false
  | VInt _ -> true
  | _ -> false  



let rec eval (env : env) (expr : expr) : value t =
  let* () = tick in
  match expr with
  | Lit (Integer n) -> return (VInt n)
  | Lit UnitVal -> return VUnit
  | Var name -> lookup_env env name

  | Lam (param, body) ->
      return (VClosure (param, body, env))

  | App (f, arg) ->
      let* f_val = eval env f in
      let* arg_val = eval env arg in
      apply f_val arg_val

  | Let (Plain, name, bound, body) ->
      let* bound_val = eval env bound in
      eval (extend_env env name bound_val) body

  | Let (Recursive, name, bound, body) ->
      
      (match bound with
       | Lam (param, lam_body) ->
           let rec_env = (name, VRecClosure (name, param, lam_body, env)) :: env in
           eval rec_env body  
       | _ -> fail (NotAFunction VUnit)) 

  | Fix fix_expr ->
      let* f_val = eval env fix_expr in
      (match f_val with
       | VClosure (fname, body, closure_env) ->
           
           (match body with
            | Lam (param, lam_body) ->
                let rec self = VRecClosure (fname, param, lam_body, (fname, self) :: closure_env) in
                return self
            | _ -> fail FixAppliedToNonFunction)
       | _ -> fail FixAppliedToNonFunction)

  | UnOp (Negate, e) ->
      let* v = eval env e in
      let* n = expect_int v in
      return (VInt (-n))

  | BinOp (op, left, right) ->
      let* l = eval env left in
      let* r = eval env right in
      (match op with
       | Add ->
           let* a = expect_int l in
           let* b = expect_int r in
           return (VInt (a + b))
       | Sub ->
           let* a = expect_int l in
           let* b = expect_int r in
           return (VInt (a - b))
       | Mul ->
           let* a = expect_int l in
           let* b = expect_int r in
           return (VInt (a * b))
       | Div ->
           let* a = expect_int l in
           let* b = expect_int r in
           if b = 0 then fail DivisionByZero
           else return (VInt (a / b)))

  | CmpOp (op, left, right) ->
      let* l = eval env left in
      let* r = eval env right in
      (match l, r with
       | VInt a, VInt b ->
           let result = 
             match op with
             | Eq -> a = b
             | Neq -> a <> b
             | Lt -> a < b
             | Le -> a <= b
             | Gt -> a > b
             | Ge -> a >= b
           in
           return (VInt (if result then 1 else 0))
       | _ -> fail (NotAnInteger l))  

  | If (cond, then_branch, else_branch) ->
      let* cond_val = eval env cond in
      (match cond_val with
       | VInt _ ->
           if is_truthy cond_val
           then eval env then_branch
           else eval env else_branch
       | _ -> fail (InvalidCondition cond_val))

and apply (f_val : value) (arg_val : value) : value t =
  match f_val with
  | VClosure (param, body, closure_env) ->
      eval (extend_env closure_env param arg_val) body

  | VRecClosure (name, param, body, closure_env) ->
      
      let new_env = extend_env closure_env param arg_val in
      let new_env = extend_env new_env name f_val in
      eval new_env body

  | VBuiltin f -> 
      (match f arg_val with
       | Ok v -> return v
       | Error e -> fail e)

  | _ -> fail (NotAFunction f_val)



let default_steps = 50_000

let eval_expr ?(steps = default_steps) expr =
  run (eval initial_env expr) steps

let string_of_value = function
  | VInt n -> string_of_int n
  | VUnit -> "()"
  | VClosure (param, _, _) -> Printf.sprintf "<fun %s>" param
  | VRecClosure (name, param, _, _) -> Printf.sprintf "<rec fun %s %s>" name param
  | VBuiltin _ -> "<builtin>"

let string_of_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | NotAnInteger v -> Printf.sprintf "Expected integer, got %s" (string_of_value v)
  | NotAFunction v -> Printf.sprintf "Expected function, got %s" (string_of_value v)
  | DivisionByZero -> "Division by zero"
  | StepLimitExceeded -> "Step limit exceeded"
  | InvalidCondition v -> Printf.sprintf "Invalid condition (expected integer): %s" (string_of_value v)
  | FixAppliedToNonFunction -> "fix applied to non-function"

let print_result = function
  | Ok v -> Printf.printf "Result: %s\n" (string_of_value v)
  | Error e -> Printf.printf "Error: %s\n" (string_of_error e)