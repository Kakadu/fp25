open Ast

type value

type error =
  | Unbound_variable of ident
  | Expected_int of value
  | Expected_bool of value
  | Expected_function of value
  | Division_by_zero
  | Invalid_recursion of ident
  | Invalid_fix of value
  | Invalid_comparison of value * value
  | Step_limit_exceeded

val string_of_value : value -> string
val string_of_error : error -> string

val eval_expr : ?fuel:int -> expr -> (value, error) result
val eval_program : ?fuel:int -> program -> (value list, error) result
