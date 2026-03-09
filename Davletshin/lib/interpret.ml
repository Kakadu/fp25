[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Utils
open Pprintast

module Interpret (M : MONADERROR) : sig
  val run : name Ast.t -> int -> output M.t
end = struct
  type env = (name * value) list

  and value =
    | VUnit
    | VInt of int
    | VClosure of name * name Ast.t * env
    | VBuiltin of (value -> value M.t)

  let ( let* ) = M.( let* )
  let vint n = M.return (VInt n)
  let vclosure n e env = M.return (VClosure (n, e, env))

  (** [eval env e] is the [<env, e> ==> v] relation. *)
  let rec eval env e steps =
    if steps <= 0
    then M.fail ProgramFreeze
    else (
      match e with
      | Int n -> vint n
      | Var x -> eval_var env x
      | Abs (x, b) -> vclosure x b env
      | App (l, r) -> eval_app env l r steps
      | Binop (op, l, r) -> eval_binop env op l r steps
      | Unop (op, e) -> eval_unop env op e steps
      | If (c, t, e) -> eval_if env c t e steps
      | Let (Nonrec, n, e1, e2) -> eval_let env n e1 e2 steps
      | Let (Rec, n, b, e2) -> eval_letrec env n b e2 steps
      | Fix e -> eval_fix env e steps)

  (** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
  and eval_var env x =
    match env with
    | [] -> M.fail (UnknownVariable x)
    | (y, v) :: rest -> if y = x then M.return v else eval_var rest x

  (** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
  and eval_app env e1 e2 steps =
    let* v1 = eval env e1 (steps - 1) in
    match v1 with
    | VClosure (n, body, defenv) ->
      let* v2 = eval env e2 (steps - 1) in
      eval ((n, v2) :: defenv) body (steps - 1)
    | VBuiltin f ->
      let* v2 = eval env e2 (steps - 1) in
      f v2
    | _ -> M.fail (TypeError "Tried to apply non-function")

  and eval_binop env op e1 e2 steps =
    let* v1 = eval env e1 (steps - 1) in
    let* v2 = eval env e2 (steps - 1) in
    match op, v1, v2 with
    | Plus, VInt a, VInt b -> vint (a + b)
    | Minus, VInt a, VInt b -> vint (a - b)
    | Times, VInt a, VInt b -> vint (a * b)
    | Divide, VInt _, VInt 0 -> M.fail DivisionByZero
    | Divide, VInt a, VInt b -> vint (a / b)
    | Eq, VInt a, VInt b -> vint (if a = b then 1 else 0)
    | Neq, VInt a, VInt b -> vint (if a <> b then 1 else 0)
    | Lt, VInt a, VInt b -> vint (if a < b then 1 else 0)
    | Gt, VInt a, VInt b -> vint (if a > b then 1 else 0)
    | Le, VInt a, VInt b -> vint (if a <= b then 1 else 0)
    | Ge, VInt a, VInt b -> vint (if a >= b then 1 else 0)
    | _ -> M.fail (TypeError "Invalid binary operation")

  and eval_unop env op e steps =
    let* v = eval env e (steps - 1) in
    match op, v with
    | Pos, VInt a -> vint (+a)
    | Neg, VInt a -> vint (-a)
    | _ -> M.fail (TypeError "Invalid unary operation")

  and eval_if env c t e steps =
    let* guard = eval env c (steps - 1) in
    match guard with
    | VInt 0 -> eval env e (steps - 1)
    | VInt _ -> eval env t (steps - 1)
    | _ -> M.fail (TypeError "if guard must be an integer")

  and eval_let env n e1 e2 steps =
    let* v1 = eval env e1 (steps - 1) in
    eval ((n, v1) :: env) e2 (steps - 1)

  and eval_letrec env n b e2 steps =
    match b with
    | Abs (x, body) ->
      let rec env' = (n, VClosure (x, body, env')) :: env in
      eval env' e2 (steps - 1)
    | body -> eval env body (steps - 1)

  and eval_fix env e steps =
    let* v = eval env e (steps - 1) in
    match v with
    | VClosure (self, body, defenv) ->
      (match body with
       | Abs (param, b) ->
         let rec new_closure = VClosure (param, b, (self, new_closure) :: defenv) in
         M.return new_closure
       | b -> eval env b (steps - 1))
    | _ -> M.fail (TypeError "Tried to apply fix to not a function")

  and print (e : value) =
    match e with
    | VUnit -> M.return OUnit
    | VInt n -> M.return (OInt n)
    | VClosure (name, body, _) -> M.return (OAbs (name, body))
    | VBuiltin _ -> M.return (OBuiltin "print")
  ;;

  let builtin_print =
    VBuiltin
      (function
        | VInt n ->
          print_int n;
          print_newline ();
          M.return VUnit
        | _ -> M.fail (TypeError "Tried to print not an integer"))
  ;;

  let initial_env = [ "print", builtin_print ]

  let run e steps =
    let* v = eval initial_env e steps in
    print v
  ;;
end

let parse_and_run str steps =
  let module I = Interpret (RESULT) in
  match Parser.parse str with
  | Ok ast ->
    (match I.run ast steps with
     | Ok n ->
       (match n with
        | OUnit -> Printf.printf "Success: Unit\n"
        | OInt n -> Printf.printf "Success: %d\n" n
        | OAbs (p, b) -> Printf.printf "Success: fun %s -> %s\n" p (ast_to_string b)
        | OBuiltin n -> Printf.printf "Success: val %s = <fun>\n" n)
     | Error err ->
       (match err with
        | UnknownVariable x -> Printf.eprintf "UnknownVariable: %s\n%!" x
        | TypeError msg -> Printf.eprintf "TypeError: %s\n%!" msg
        | DivisionByZero -> Printf.eprintf "Division by zero\n%!"
        | ProgramFreeze -> Printf.eprintf "ProgramFreeze\n%!"))
  | Error _ ->
    Printf.eprintf "Parsing error\n%!";
    exit 1
;;
