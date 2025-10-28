open Ast

type error =
  | Not_implemented of string
  | Occurs_check of string * ty
  | Unification_failed of ty * ty
  | Type_mismatch
  | Unbound_value of string

val show_error : error -> string
val pp_error : Format.formatter -> error -> unit

module Infer (_ : Monads.STATE_MONAD) : sig
  val expression : expression -> (Ast.ty, error) Result.t
end
