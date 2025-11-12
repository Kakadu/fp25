type error =
  | Not_implemented of string
  | Occurs_check of Ident.t * Typedtree.ty
  | Unification_failed of Typedtree.ty * Typedtree.ty
  | Type_mismatch
  | Unbound_value of string

val show_error : error -> string
val pp_error : Format.formatter -> error -> unit

module Infer (_ : Monads.STATE_MONAD) : sig
  val expression : Parsetree.expression -> (Typedtree.ty, error) Result.t
end
