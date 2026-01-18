[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

module type ERROR = sig
  type t
end

module Make (M : Monads.STATE_MONAD) (Error : ERROR) = struct
  type structure = value_binding * value_binding list
  and value_binding = Parsetree.pattern * value

  and value =
    | VConstant of Parsetree.constant
    | VTuple of value * value * value list
    | VFun of Parsetree.pattern * Parsetree.expression * environment
    | VConstruct of string * value option
    (** [ Some 123, None, Ok (fun x -> x + 1) etc. ] *)
    | VPrimitive of string * (value -> (state, value, Error.t) M.t)
    (** [ print_value ] is presented as [ VPrimitive ("print_value", implementation) ] *)

  and environment = (string, value, Base.String.comparator_witness) Base.Map.t
  and state = int

  let vprimitive name impl = VPrimitive (name, impl)

  open Base
  open Base.Format
  open Pprint
  open Pprint.Parens

  let rec show_value ?(ctx = Free) = function
    | VConstant x -> sprintf "%s" (Parsetree.show_constant x)
    | VTuple (a, b, xs) ->
      set_parens ~ctx [ Tuple; App; Binop ]
      @@ (show_tuple (show_value ~ctx:Tuple)) (a, b, xs)
    | VConstruct ("::", Some (VTuple (head, tail, []))) ->
      let rec helper acc = function
        | VConstruct ("::", Some (VTuple (hd, tl, []))) -> helper (hd :: acc) tl
        | VConstruct ("[]", None) ->
          show_list_brackets (show_value ~ctx:Binop) (head :: List.rev acc)
        | _ as exp -> show_list_cons show_value (head :: List.rev (exp :: acc))
      in
      helper [] tail
    | VConstruct (name, None) -> sprintf "%s" name
    | VConstruct (name, Some arg) -> sprintf "@[%s (%s)@]" name (show_value arg)
    | VFun (patt, expr, _) ->
      sprintf
        "(fun %s -> %s)"
        (Parsetree.show_pattern patt)
        (Parsetree.show_expression expr)
    | VPrimitive (name, _) -> name
  ;;

  let pp_value ppf value = fprintf ppf "%s" (show_value value)
end
