[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* adds parentheses according to the current context and a given set of contexts where they are required *)
module Parens : sig
  (* allows explicitly enumerating contexts where parentheses are required *)
  type context =
    | Free (** top-level [ let ] and expressions after [ in ] are free *)
    | Binop (** [ XXX + XXX * (XXX - XXX) ] *)
    | Tuple (** [ XXX, XXX ]*)
    | App (** [ XXX XXX ] *)
    | LeftSideFun (** [ fun XXX -> ... ] *)
    | RightSideFun (** [ fun ... -> XXX ] *)
    | MatchWith (** [ match XXX with ... ] *)
    | LeftSideMatch (** [ match ... with | XXX -> ... ] *)
    | RightSideMatch (** [ match ... with | ... -> XXX ] *)
    | LeftSideArrow (** [ 'XXX -> ... ] in type declaration and infered types *)
    | RightSideArrow (** [ ... -> 'XXX ] in type declaration and infered types *)
    | LeftSideLet (** [ let XXX = ... in ... ] *)
    | RightSideLet (** [ let ... = XXX in ... ] *)
    | Ite (** [ if XXX then XXX else XXX ] *)

  (** [ ctx ] - current context; if it's in the provided list of contexts, parentheses will be added *)
  val set_parens : ctx:context -> context list -> string -> string
end

(* aux stuff for pretty-printers *)

val show_many : sep:string -> ('a -> string) -> 'a list -> string
val show_tuple : ('a -> string) -> 'a * 'a * 'a list -> string
val show_list_brackets : ('a -> string) -> 'a list -> string

val show_list_cons
  :  ?ctx:Parens.context
  -> (?ctx:Parens.context -> 'a -> string)
  -> 'a list
  -> string
