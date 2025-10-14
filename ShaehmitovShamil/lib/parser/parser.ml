open Ast
open Base
open Angstrom

let whitespace = take_while Char.is_whitespace

let keyword s = string s <* whitespace

