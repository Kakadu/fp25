[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

(** Function to check for whitespace characters *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(** Skips any number of spaces *)
let spaces = skip_while is_space

(** Requires at least one space *)
let spaces1 = take_while1 is_space >>= fun _ -> return ()

(** This is a reserved word *)
let parens p = char '(' *> spaces *> p <* spaces <* char ')'

let braces p = char '{' *> spaces *> p <* spaces <* char '}'

let is_keyword = function
  | "let" | "in" | "fun" | "true" | "false" | "rec" | "else" | "if" | "then" | "_" -> true
  | _ -> false
;;
