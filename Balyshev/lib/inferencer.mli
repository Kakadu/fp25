type error =
  | Not_implemented of string
  | Occurs_check of Typedtree.ty * Typedtree.ty
  | Unification_failed of Typedtree.ty * Typedtree.ty
  | Type_mismatch
  | Unbound_value of string
  | Type_was_not_declared of string
  | Type_arity_mismatch of string (* TODO : change message *)
  | Unbound_type_variable of string
  | Type_param_duplicates of string
  | Unbound_constructor of string
  | Type_env_invariant_violation of string
  | Constructor_arity_mismatch of string
  | Only_variables_on_the_left_of_let_rec
  | Only_functions_on_the_right_of_let_rec

val show_error : error -> string
val pp_error : Format.formatter -> error -> unit

module Infer (_ : Monads.STATE_MONAD) : sig
  val expression : ?debug:bool -> Parsetree.expression -> (Typedtree.ty, error) result

  val structure
    :  ?debug:bool
    -> Parsetree.structure_item * Parsetree.structure_item list
    -> (Typedtree.structure_item * Typedtree.structure_item list, error) result
end
