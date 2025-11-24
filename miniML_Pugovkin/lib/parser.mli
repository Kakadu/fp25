open Ast

exception Error of string

val expr_of_string : string -> expr
val toplevel_of_string : string -> toplevel
val program_of_string : string -> program
