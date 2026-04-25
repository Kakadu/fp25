[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Utils

type state = int [@@deriving show]

type ereclabel =
  | ENonrec
  | ERec of string
[@@deriving show]

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of closure
  | VUnit

and closure =
  { param : string
  ; body : expr
  ; env : value Table.t
  ; label : ereclabel
  }
[@@deriving show]

type toplevel_value =
  | VLet of string * value
  | VExpr of value

type runtime_error =
  | RUnboundValue of string
  | RNotAFunction of value
  | RDivisionByZero
  | RTypeMismatch
  | RInvalidOperand
  | RIfCondNotBool
  | RStepLimitExceeded
  | REvalError

let pp_runtime_error fmt = function
  | RUnboundValue x -> Format.fprintf fmt "Unbound value: %s" x
  | RNotAFunction v -> Format.fprintf fmt "This is not a function: %s" (show_value v)
  | RDivisionByZero -> Format.fprintf fmt "division by zero"
  | RTypeMismatch -> Format.fprintf fmt "type mismatch"
  | RInvalidOperand -> Format.fprintf fmt "operand type mismatch"
  | RIfCondNotBool -> Format.fprintf fmt "condition must be bool"
  | RStepLimitExceeded -> Format.fprintf fmt "step limit exceeded"
  | REvalError -> Format.fprintf fmt "evaluation error"
;;

type 'a evalres =
  | EFailed of runtime_error
  | EOk of state * 'a
[@@deriving show]

let pp_value fmt = function
  | VInt x -> Format.fprintf fmt "%d" x
  | VFloat x -> Format.fprintf fmt "%f" x
  | VBool x -> Format.fprintf fmt "%b" x
  | VClosure _ -> Format.fprintf fmt "<fun>"
  | VUnit -> Format.fprintf fmt "()"
;;

let pp_toplevel_value fmt = function
  | VExpr v -> Format.fprintf fmt "- : %a" pp_value v
  | VLet (x, v) -> Format.fprintf fmt "val %s = %a" x pp_value v
;;

let ( >>= ) m f st =
  match m st with
  | EFailed s -> EFailed s
  | EOk (st', x) -> f x st'
;;

let ( let* ) = ( >>= )
let return x st = EOk (st, x)
let read st = EOk (st, st)
let write st (_ : state) = EOk (st, ())
let fail s _ = EFailed s
let run f st = f st

let step =
  let* remaining_steps = read in
  if remaining_steps <= 0 then fail RStepLimitExceeded else write (remaining_steps - 1)
;;

let lookup env x =
  match Table.lookup x env with
  | Some res -> return res
  | None -> fail (RUnboundValue x)
;;

let make_closure env param body label = VClosure { param; body; env; label }

let rec eval env exp =
  let* () = step in
  match exp with
  | EConst (IConst c) -> return (VInt c)
  | EConst (FConst c) -> return (VFloat c)
  | EConst (BConst c) -> return (VBool c)
  | EVar x ->
    let* v = lookup env x in
    return v
  | EBinOp (op, l, r) -> eval_binop env op l r
  | EIf (cond, e1, e2) -> eval_if env cond e1 e2
  | EFun (EVar x, e) -> return (make_closure env x e ENonrec)
  | ELet (Nonrecursive, Bind (EVar x, e1), e2) ->
    let* v1 = eval env e1 in
    let env' = Table.extend x v1 env in
    let* res = eval env' e2 in
    return res
  | ELet (Recursive, Bind (EVar x, e1), e2) ->
    let* v1 = eval (Table.extend x VUnit env) e1 in
    let v1' =
      match v1 with
      | VClosure { param; body; env; _ } -> VClosure { param; body; env; label = ERec x }
      | v -> v
    in
    let env' = Table.extend x v1' env in
    let* res = eval env' e2 in
    return res
  | EApp (e1, e2) ->
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    apply_closure v1 v2
  | _ -> fail REvalError

and eval_binop env op e1 e2 =
  let* v1 = eval env e1 in
  let* v2 = eval env e2 in
  let have_same_val_type l r =
    match l, r with
    | VBool _, VBool _ | VInt _, VInt _ | VFloat _, VFloat _ -> true
    | _ -> false
  in
  match op, v1, v2 with
  | Add, VInt l, VInt r -> return (VInt (l + r))
  | Sub, VInt l, VInt r -> return (VInt (l - r))
  | Mul, VInt l, VInt r -> return (VInt (l * r))
  | Div, VInt l, VInt r -> if r = 0 then fail RDivisionByZero else return (VInt (l / r))
  | AddF, VFloat l, VFloat r -> return (VFloat (l +. r))
  | SubF, VFloat l, VFloat r -> return (VFloat (l -. r))
  | MulF, VFloat l, VFloat r -> return (VFloat (l *. r))
  | DivF, VFloat l, VFloat r ->
    if r = 0. then fail RDivisionByZero else return (VFloat (l /. r))
  | Eq, l, r when have_same_val_type l r -> return (VBool (l = r))
  | Neq, l, r when have_same_val_type l r -> return (VBool (l <> r))
  | Lt, l, r when have_same_val_type l r -> return (VBool (l < r))
  | Leq, l, r when have_same_val_type l r -> return (VBool (l <= r))
  | Gt, l, r when have_same_val_type l r -> return (VBool (l > r))
  | Geq, l, r when have_same_val_type l r -> return (VBool (l >= r))
  | And, VBool l, VBool r -> return (VBool (l && r))
  | Or, VBool l, VBool r -> return (VBool (l || r))
  | _ -> fail RInvalidOperand

and eval_if env cond e1 e2 =
  let* () = step in
  let* cond' = eval env cond in
  match cond' with
  | VBool true -> eval env e1
  | VBool false -> eval env e2
  | _ -> fail RIfCondNotBool

and apply_closure vfun varg =
  let* () = step in
  match vfun with
  | VClosure { param; body; env; label = ENonrec } ->
    let defenv' = Table.extend param varg env in
    eval defenv' body
  | VClosure { param; body; env; label = ERec x } ->
    let defenv' = Table.extend param varg (Table.extend x vfun env) in
    eval defenv' body
  | _ -> fail (RNotAFunction vfun)
;;

let eval_toplevel env tl =
  match tl with
  | TopExpr e ->
    let* v = eval env e in
    return (env, VExpr v)
  | TopLet (Nonrecursive, Bind (EVar x, e)) ->
    let* v = eval env e in
    let env' = Table.extend x v env in
    return (env', VLet (x, v))
  | TopLet (Recursive, Bind (EVar x, e)) ->
    let env' = Table.extend x VUnit env in
    let* v = eval env' e in
    let v' =
      match v with
      | VClosure c -> VClosure { c with label = ERec x }
      | _ -> v
    in
    let env'' = Table.extend x v' env in
    return (env'', VLet (x, v'))
  | _ -> fail REvalError
;;

let run_eval tl env steps = run (eval_toplevel env tl) steps
