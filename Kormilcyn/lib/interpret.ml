[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Utils
open Ast

type error =
  [ `NotAValue of string
  | `Type_error of string
  | `Unbound of string
  | `Division_by_zero
  ]

let pp_error ppf = function
  | `NotAValue msg -> Format.fprintf ppf "Not a value: %s" msg
  | `Type_error msg -> Format.fprintf ppf "Type error: %s" msg
  | `Unbound x -> Format.fprintf ppf "Unbound variable %s" x
  | `Division_by_zero -> Format.fprintf ppf "Division by zero"
;;

module Interpret (M : MONAD_FAIL) : sig
  val run : int -> 'name Ast.t -> (int, [> error ]) M.t
end = struct
  let ( let* ) m f = M.bind m ~f
  let return = M.return
  let fail = M.fail

  type 'name value =
    | VInt of int
    | VClosure of 'name * 'name Ast.t * 'name env
    | VFix

  and 'name env = ('name * 'name value) list

  let run max_steps (e : 'name Ast.t) : (int, [> error ]) M.t =
    let rec resolve env x =
      match env with
      | [] -> None
      | (y, v) :: rest -> if y = x then Some v else resolve rest x
    and eval (env : 'name env) (e : 'name Ast.t) (steps : int)
      : ('name value, [> error ]) M.t
      =
      if steps > max_steps
      then fail (`NotAValue "Steps limit exceeded")
      else (
        match e with
        | Int i -> return (VInt i)
        | Var x -> eval_var env x
        | Neg e -> eval_neg env e (steps + 1)
        | Bin (bop, e1, e2) -> eval_bin env bop e1 e2 (steps + 1)
        | Let (x, e1, e2) -> eval_let env x e1 e2 (steps + 1)
        | If (e1, e2, e3) -> eval_if env e1 e2 e3 (steps + 1)
        | Fun (x, e) -> return (VClosure (x, e, env))
        | App (e1, e2) -> eval_app env e1 e2 (steps + 1)
        | Fix -> return VFix
        | LetRec (f, Fun (x, b), e2) -> eval_letrec env f x b e2 (steps + 1)
        | LetRec _ -> fail (`Type_error "let rec expects a function on the right"))
    and eval_var env x =
      match resolve env x with
      | Some v -> return v
      | None -> fail (`Unbound "<unbound>")
    and eval_neg env e steps : ('name value, [> error ]) M.t =
      let* v = eval env e (steps + 1) in
      match v with
      | VInt x -> return (VInt (-x))
      | _ -> fail (`Type_error "unary - expects an integer")
    and eval_bin env bop e1 e2 steps : ('name value, [> error ]) M.t =
      let* v1 = eval env e1 (steps + 1) in
      let* v2 = eval env e2 (steps + 1) in
      match bop, v1, v2 with
      | Add, VInt a, VInt b -> return (VInt (a + b))
      | Sub, VInt a, VInt b -> return (VInt (a - b))
      | Mul, VInt a, VInt b -> return (VInt (a * b))
      | Div, VInt _, VInt 0 -> fail `Division_by_zero
      | Div, VInt a, VInt b -> return (VInt (a / b))
      | Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
      | Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
      | Geq, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
      | _ -> fail (`Type_error "binary operation expects integers")
    and eval_let env x e1 e2 steps : ('name value, [> error ]) M.t =
      let* v1 = eval env e1 (steps + 1) in
      eval ((x, v1) :: env) e2 (steps + 1)
    and eval_if env e1 e2 e3 steps : ('name value, [> error ]) M.t =
      let* vcond = eval env e1 (steps + 1) in
      match vcond with
      | VInt 1 -> eval env e2 (steps + 1)
      | VInt 0 -> eval env e3 (steps + 1)
      | _ -> fail (`Type_error "if expects an integer condition")
    and eval_app env e1 e2 steps : ('name value, [> error ]) M.t =
      let* vf = eval env e1 (steps + 1) in
      match vf with
      | VClosure (x, body, defenv) ->
        let* varg = eval env e2 (steps + 1) in
        eval ((x, varg) :: defenv) body (steps + 1)
      | VFix -> eval_fix env e2 steps
      | _ -> fail (`Type_error "application of a non-function")
    and eval_fix env f steps =
      let* vfun = eval env f (steps + 1) in
      match vfun with
      | VClosure (farg, fbody, fenv) ->
        (match fbody with
         | Fun (x, body) ->
           let rec self = VClosure (x, body, (farg, self) :: fenv) in
           return self
         | _ -> fail (`Type_error "fix expects a function returning a function"))
      | _ -> fail (`Type_error "fix expects a function")
    and eval_letrec env f x b e2 steps : ('name value, [> error ]) M.t =
      let rec env' = (f, VClosure (x, b, env')) :: env in
      eval env' e2 (steps + 1)
    and int_of_value = function
      | VInt i -> return i
      | VClosure _ -> fail (`NotAValue "cannot represent function as integer")
      | VFix -> fail (`NotAValue "cannot represent fix as a value")
    in
    let* v = eval [] e 0 in
    int_of_value v
  ;;
end

let parse_and_run ?(max_steps = 10000) str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run max_steps) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error err ->
    (match err with
     | #Parser.error as e -> Format.eprintf "Parsing error: %a\n%!" Parser.pp_error e
     | #error as e -> Format.eprintf "Interpreter error: %a\n%!" pp_error e)
;;
