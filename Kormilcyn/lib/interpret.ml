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

module Interpret (M : MONAD_FAIL) : sig
  val run : _ Ast.t -> (int, [> error ]) M.t
end = struct
  (* TODO: step counting *)

  let ( let* ) m f = M.bind m ~f
  let return = M.return
  let fail = M.fail

  type 'name value =
    | VInt of int
    | VClosure of 'name * 'name Ast.t * 'name env

  and 'name env = ('name * 'name value) list

  let rec resolve env x =
    match env with
    | [] -> None
    | (y, v) :: rest -> if y = x then Some v else resolve rest x
  ;;

  let rec eval (env : 'name env) (e : 'name Ast.t) : ('name value, [> error ]) M.t =
    match e with
    | Int i -> return (VInt i)
    | Var x -> eval_var env x
    | Neg e -> eval_neg env e
    | Bin (bop, e1, e2) -> eval_bin env bop e1 e2
    | Let (x, e1, e2) -> eval_let env x e1 e2
    | If (e1, e2, e3) -> eval_if env e1 e2 e3
    | Fun (x, e) -> return (VClosure (x, e, env))
    | App (e1, e2) -> eval_app env e1 e2
    | LetRec (f, Fun (x, b), e2) -> eval_letrec env f x b e2
    | LetRec _ -> fail (`Type_error "let rec expects a function on the right")

  and eval_var env x =
    match resolve env x with
    | Some v -> return v
    | None -> fail (`Unbound "<unbound variable>")

  and eval_neg env e : ('name value, [> error ]) M.t =
    let* v = eval env e in
    match v with
    | VInt x -> return (VInt (-x))
    | _ -> fail (`Type_error "unary - expects an integer")

  and eval_bin env bop e1 e2 : ('name value, [> error ]) M.t =
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    match bop, v1, v2 with
    | Add, VInt a, VInt b -> return (VInt (a + b))
    | Sub, VInt a, VInt b -> return (VInt (a - b))
    | Mul, VInt a, VInt b -> return (VInt (a * b))
    | Div, VInt _, VInt 0 -> fail `Division_by_zero
    | Div, VInt a, VInt b -> return (VInt (a / b))
    | Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
    | Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
    | _ -> fail (`Type_error "binary operation expects integers")

  and eval_let env x e1 e2 : ('name value, [> error ]) M.t =
    let* v1 = eval env e1 in
    eval ((x, v1) :: env) e2

  and eval_if env e1 e2 e3 : ('name value, [> error ]) M.t =
    let* vcond = eval env e1 in
    match vcond with
    | VInt 1 -> eval env e2
    | VInt 0 -> eval env e3
    | _ -> fail (`Type_error "if expects an integer condition")

  and eval_app env e1 e2 : ('name value, [> error ]) M.t =
    let* vf = eval env e1 in
    match vf with
    | VClosure (x, body, defenv) ->
      let* varg = eval env e2 in
      eval ((x, varg) :: defenv) body
    | _ -> fail (`Type_error "application of a non-function")

  and eval_letrec env f x b e2 : ('name value, [> error ]) M.t =
    let rec env' = (f, VClosure (x, b, env')) :: env in
    eval env' e2
  ;;

  let int_of_value = function
    | VInt i -> return i
    | VClosure _ -> fail (`NotAValue "cannot represent function as integer")
  ;;

  let run e =
    let* v = eval [] e in
    int_of_value v
  ;;
end

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!";
    exit 1
  | Result.Error #error ->
    Format.eprintf "Interpreter error\n%!";
    exit 1
;;
