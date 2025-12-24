open Ast

val pp_expr : Format.formatter -> expr -> unit
val pp_toplevel : Format.formatter -> toplevel -> unit
val pp_program : Format.formatter -> program -> unit

val string_of_expr : expr -> string
val string_of_toplevel : toplevel -> string
val string_of_program : program -> string
