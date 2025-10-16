[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type value =
  | VConst of const
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * expression * environment
  | VOption of value option
  | VPrintInt

and error =
  [ `Is_not_a_function of string
  | `Unbound_value of string
  | `Type_mismatch of expression
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

let eval_binop op a b =
  match op, a, b with
  | Add, VConst (CInt a), VConst (CInt b) -> Res.return (VConst (CInt (a + b)))
  | Sub, VConst (CInt a), VConst (CInt b) -> Res.return (VConst (CInt (a - b)))
  | Mul, VConst (CInt a), VConst (CInt b) -> Res.return (VConst (CInt (a * b)))
  | _ -> failwith "not implemented"
;;

let rec eval_expression env = function
  | EVar name -> find_exn env name
  | EConst (_ as x) -> Res.return (VConst x)
  | EBinop (op, a, b) ->
    let* a = eval_expression env a in
    let* b = eval_expression env b in
    eval_binop op a b
  | _ -> failwith "not implemented"
;;

let eval_expr expr = eval_expression init_env expr

open Format

let rec show_value = function
  | VConst CUnit -> sprintf "()"
  | VConst (CInt x) -> sprintf "%d" x
  | VConst (CBool x) -> sprintf "%b" x
  | VOption None -> sprintf "None"
  | VOption (Some ex) -> sprintf "@[Some (%s)@]" (show_value ex)
  | VTuple (a, b, xs) ->
    Stdlib.List.map (fun x -> show_value x) (a :: b :: xs)
    |> String.concat ~sep:", "
    |> sprintf "(%s)"
  | _ -> failwith "not implemented"
;;

let pp_value ppf value = fprintf ppf "%s" (show_value value)

let show_error : error -> string = function
  | `Is_not_a_function name -> sprintf "%s is not a function, it can not be applied" name
  | `Unbound_value name -> sprintf "unbound value: %s" name
  | `Type_mismatch expr -> sprintf "type mismatch in %s" (Ast.show_expression expr)
  | `Division_by_zero -> sprintf "division by zero"
;;

let pp_error ppf error = fprintf ppf "%s" (show_error error)
