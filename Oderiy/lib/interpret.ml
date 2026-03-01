[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

type error =
  [ `UnknownVariable of string
  | `Type_error of string
  | `Division_by_zero
  | `Steps_exceeded
  ]

let pp_error ppf = function
  | `UnknownVariable s -> Stdlib.Format.fprintf ppf "Unbound variable: %s" s
  | `Type_error s -> Stdlib.Format.fprintf ppf "%s" s
  | `Division_by_zero -> Stdlib.Format.fprintf ppf "Division by zero"
  | `Steps_exceeded -> Stdlib.Format.fprintf ppf "Steps limit exceeded"
;;

module StateResult = struct
  type ('a, 'e) t = int -> (('a * int, 'e) result[@warning "-3"])

  let return x : ('a, 'e) t = fun steps -> Ok (x, steps)
  let fail e : ('a, 'e) t = fun _steps -> Error e

  let bind (m : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    fun steps ->
    match m steps with
    | Error e -> Error e
    | Ok (x, s') -> f x s'
  ;;

  let ( >>= ) = bind
  let get_state : (int, 'e) t = fun steps -> Ok (steps, steps)
  let put_state new_steps : (unit, 'e) t = fun _old_steps -> Ok ((), new_steps)

  let run_computation (m : ('a, 'e) t) initial_steps =
    match m initial_steps with
    | Ok (x, _) -> Ok x
    | Error e -> Error e
  ;;
end

open Ast

type value =
  | VNum of int
  | VFun of string * string Ast.t * env

and env = (string * value) list

let rec find_var name = function
  | [] -> None
  | (n, v) :: _ when String.equal n name -> Some v
  | _ :: rest -> find_var name rest
;;

let run limit expr =
  let tick limit =
    let open StateResult in
    get_state
    >>= fun steps ->
    let new_steps = steps + 1 in
    if new_steps > limit
    then fail `Steps_exceeded
    else put_state new_steps >>= fun () -> return ()
  in
  let rec interp limit bindings term =
    let open StateResult in
    tick limit
    >>= fun () ->
    match term with
    | Int n -> return (VNum n)
    | Var name ->
      (match find_var name bindings with
       | Some v -> return v
       | None -> fail (`UnknownVariable name))
    | Fun (arg, body) -> return (VFun (arg, body, bindings))
    | App (fn, arg) ->
      interp limit bindings fn
      >>= fun fn_val ->
      interp limit bindings arg
      >>= fun arg_val ->
      (match fn_val with
       | VFun (param, body, closure) -> interp limit ((param, arg_val) :: closure) body
       | _ -> fail (`Type_error "applying non-function"))
    | Neg expr ->
      interp limit bindings expr
      >>= (function
       | VNum n -> return (VNum (-n))
       | _ -> fail (`Type_error "negating non-number"))
    | Bin (op, e1, e2) ->
      interp limit bindings e1
      >>= fun v1 ->
      interp limit bindings e2
      >>= fun v2 ->
      (match v1, v2 with
       | VNum a, VNum b -> apply_binop op a b
       | _ -> fail (`Type_error "binary op on non-numbers"))
    | Let (name, rhs, body) ->
      interp limit bindings rhs
      >>= fun rhs_val -> interp limit ((name, rhs_val) :: bindings) body
    | LetRec (fn_name, Fun (param, fn_body), expr) ->
      let rec rec_bindings = (fn_name, VFun (param, fn_body, rec_bindings)) :: bindings in
      interp limit rec_bindings expr
    | LetRec _ -> fail (`Type_error "letrec body must be a function")
    | If (cond, yes, no) ->
      interp limit bindings cond
      >>= (function
       | VNum 0 -> interp limit bindings no
       | VNum _ -> interp limit bindings yes
       | _ -> fail (`Type_error "condition must be number"))
    | Fix ->
      let fix_body =
        Fun ("__x", App (App (Var "__f", App (Fix, Var "__f")), Var "__x"))
      in
      return (VFun ("__f", fix_body, bindings))
  and apply_binop op x y =
    let open StateResult in
    match op with
    | Add -> return (VNum (x + y))
    | Sub -> return (VNum (x - y))
    | Mul -> return (VNum (x * y))
    | Div -> if y = 0 then fail `Division_by_zero else return (VNum (x / y))
    | Lt -> return (VNum (if x < y then 1 else 0))
    | Leq -> return (VNum (if x <= y then 1 else 0))
    | Eq -> return (VNum (if x = y then 1 else 0))
    | Geq -> return (VNum (if x >= y then 1 else 0))
    | Gt -> return (VNum (if x > y then 1 else 0))
  in
  let open StateResult in
  interp limit [] expr
  >>= (function
         | VNum n -> return n
         | VFun _ -> fail (`Type_error "result is function, not number"))
  |> fun computation -> run_computation computation 0
;;

let parse_and_run ?(max_steps = 10000) input =
  match Parser.parse input with
  | Error parse_err ->
    Stdlib.Format.eprintf "%a\n%!" Parser.pp_error parse_err;
    Stdlib.exit 1
  | Ok program ->
    (match run max_steps program with
     | Ok result -> Stdlib.Printf.printf "Success: %d\n" result
     | Error (#error as eval_err) ->
       Stdlib.Format.eprintf "Error: %a\n%!" pp_error eval_err;
       Stdlib.exit 1)
;;
