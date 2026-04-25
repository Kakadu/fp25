open Ast

type input = char list

type 'a parse_result =
  | Failed of string
  | Parsed of 'a * input

type 'a parser = input -> 'a parse_result

val parser : string -> toplevel parse_result
