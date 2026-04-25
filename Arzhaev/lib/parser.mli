open Ast

type parse_error =
  | PExpectedInt
  | PExpectedFloat
  | PExpectedId
  | PExpectedRightOperand
  | PSyntaxError

val pp_parse_error : Format.formatter -> parse_error -> unit

type input = char list

type 'a parse_result =
  | Failed of parse_error
  | Parsed of 'a * input

type 'a parser = input -> 'a parse_result

val parser : string -> toplevel parse_result
