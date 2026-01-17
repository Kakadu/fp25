module Error : sig
  type t =
    | Is_not_a_function of Parsetree.expression
    | Unbound_value of string
    | Type_mismatch of string
    | Division_by_zero
    | Not_implemented of string
    | LeftSideRec of string
    | RightSideRec of string
    | Match_failure

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Make (M : Monads.STATE_MONAD) : sig
  open Valuetree.Make(M)(Error)

  val eval_expression : Parsetree.expression -> (value, Error.t) Result.t

  val eval_structure
    :  Parsetree.value_binding * Parsetree.value_binding list
    -> (structure, Error.t) Result.t
end
