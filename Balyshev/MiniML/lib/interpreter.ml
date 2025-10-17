[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type value =
  | VConstant of constant
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * expression * environment
  | VConstruct of string * value option

and error =
  [ `Is_not_a_function of string
  | `Unbound_value of string
  | `Type_mismatch
  | `Division_by_zero
  ]

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

open Base

module Res = struct
  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    match monad with
    | Ok result -> f result
    | Error x -> fail x
  ;;

  let ( let* ) = ( >>= )
end

open Res

let init_env : (string, value, Base.String.comparator_witness) Base.Map.t =
  Base.Map.empty (module Base.String)
;;

let find_exn env key =
  match Map.find env key with
  | Some value -> Res.return value
  | None -> Res.fail (`Unbound_value key : error)
;;

let eval_binop op a b : (value, error) result =
  match op, a, b with
  | Add, VConstant (CInt a), VConstant (CInt b) -> Res.return (VConstant (CInt (a + b)))
  | Sub, VConstant (CInt a), VConstant (CInt b) -> Res.return (VConstant (CInt (a - b)))
  | Mul, VConstant (CInt a), VConstant (CInt b) -> Res.return (VConstant (CInt (a * b)))
  | Div, VConstant (CInt a), VConstant (CInt b) ->
    if b = 0 then fail `Division_by_zero else Res.return (VConstant (CInt (a * b)))
  | Cons, left, right -> Res.return (VConstruct ("Cons", Some (VTuple (left, right, []))))
  | _ -> failwith "not implemented"
;;

let eval_tuple env eval (a, b, xs) =
  let* a = eval env a in
  let* b = eval env b in
  let rec helper items acc =
    match items with
    | [] -> return (List.rev acc)
    | x :: xs ->
      let* exp = eval env x in
      helper xs (exp :: acc)
  in
  let* xs = helper xs [] in
  return (VTuple (a, b, xs))
;;

let rec bind_to_env env eval patt value : (environment, error) result =
  match patt, value with
  | PAny, _ -> Res.return env
  | PVar x, value -> Map.set env ~key:x ~data:value |> Res.return
  | PConstruct (name, Some patt), value ->
    (match value with
     | VConstruct (name2, Some value) when String.equal name name2 ->
       bind_to_env env eval patt value
     | _ -> fail `Type_mismatch)
  | PTuple (p1, p2, ps), VTuple (v1, v2, vs) ->
    let rec helper env = function
      | [], [] -> Res.return env
      | p :: ps, v :: vs ->
        let* env' = bind_to_env env eval p v in
        helper env' (ps, vs)
      | _ -> fail `Type_mismatch
    in
    helper env (p1 :: p2 :: ps, v1 :: v2 :: vs)
  | _ -> failwith "not implemented"
;;

let rec eval_expression env = function
  | EVar name -> find_exn env name
  | EConstant (_ as x) -> Res.return (VConstant x)
  | EBinop (op, a, b) ->
    let* a = eval_expression env a in
    let* b = eval_expression env b in
    eval_binop op a b
  | ETuple (a, b, xs) -> eval_tuple env eval_expression (a, b, xs)
  | EConstruct (name, None) -> Res.return (VConstruct (name, None))
  | EConstruct (name, Some arg) ->
    let* arg = eval_expression env arg in
    Res.return (VConstruct (name, Some arg))
  | ELet (NonRecursive, patt, expr, body) ->
    let* value = eval_expression env expr in
    let* env' = bind_to_env env eval_expression patt value in
    eval_expression env' body
  | ELet (Recursive, _, _, _) | _ -> failwith "not implemented"
;;

let eval_expr expr = eval_expression init_env expr

open Format

let rec show_value = function
  | VConstant CUnit -> sprintf "()"
  | VConstant (CInt x) -> sprintf "%d" x
  | VConstant (CBool x) -> sprintf "%b" x
  | VTuple (a, b, xs) ->
    Stdlib.List.map (fun x -> show_value x) (a :: b :: xs)
    |> String.concat ~sep:", "
    |> sprintf "@[(%s)@]"
  | VConstruct ("Cons", Some (VTuple (a, b, []))) ->
    sprintf "@[(%s :: %s)@]" (show_value a) (show_value b)
  | VConstruct ("Nil", None) -> sprintf "[]"
  | VConstruct (name, None) -> sprintf "%s" name
  | VConstruct (name, Some arg) -> sprintf "@[%s (%s)@]" name (show_value arg)
  | _ -> failwith "not implemented"
;;

let pp_value ppf value = fprintf ppf "%s" (show_value value)

let show_error : error -> string = function
  | `Is_not_a_function name -> sprintf "%s is not a function, it can not be applied" name
  | `Unbound_value name -> sprintf "unbound value: %s" name
  | `Type_mismatch -> sprintf "type mismatch"
  | `Division_by_zero -> sprintf "division by zero"
;;

let pp_error ppf error = fprintf ppf "%s" (show_error error)
