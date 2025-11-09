open Type
open Ast
open Monads
(* 135 ml implementation:  tapl *)

type term =
  | TInt of int
  | TBool of bool
  | TVar of int
  | TAbs of term
  | TApp of term * term
[@@deriving show { with_path = false }]

type error =
  [ `UnifyError of ty * ty
  | `AbstractionExpected of ty
  ]

module Context = struct
  type t = context
end

(* Gamma *)
module Scheme : sig
  type t

  val mono : ty -> scheme
end = struct
  type t = scheme

  let mono t = ISet.empty, t
end

module Result : FailMonad = struct
  type ('a, 'b) t = ('b, 'a) Result.t

  let fail e = Result.Error e
  let return a = Result.Ok a
  let bind o f = Result.bind o f

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end
end

open Result
open Result.Syntax

let rec unify e1 e2 =
  match e1, e2 with
  | TGround g1, TGround g2 when g1 = g2 -> return (e1, e2)
  | TVar b1, TVar b2 when b1 = b2 -> return (e1, e2)
  | TGround _, TGround _ -> fail (`UnifyError (e1, e2))
  | TArrow (l1, l2), TArrow (r1, r2) ->
    let* l1, r1 = unify l1 r1 in
    let* l2, r2 = unify l2 r2 in
    return (tarrow l1 r1, tarrow l2 r2)
  | _ -> fail (`UnifyError (e1, e2))
;;

let infer =
  fun x ->
  let rec helper = function
    | EConst (Bool _) -> return tbool
    | EConst (Int _) -> return tint
    | EBop (_, e1, e2) ->
      let* e1 = helper e1 in
      let* e2 = helper e2 in
      let* e1, e2 = unify e1 e2 in
      return (tarrow e1 e2)
    | EVar b -> failwith "not impl: lookup enviroment needed"
    | ELet (_, v, e1, e2) -> failwith "not impl"
    | EAbs (v, e2) -> failwith "not impl"
    | EApp (e1, e2) ->
      let* e1 = helper e1 in
      let* e2 = helper e2 in
      (match e1 with
       | TArrow (l, r) ->
         let* _ = unify l e2 in
         return r
       | _ -> fail (`AbstractionExpected e1))
  in
  return (helper x)
;;
