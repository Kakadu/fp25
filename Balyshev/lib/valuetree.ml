[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

module type ERROR = sig
  type t
end

(* represent program after interpreter processing *)
module Make (M : Monads.STATE_MONAD) (Error : ERROR) = struct
  (* counter for generating unique Ident.t for value bindings and local bindings *)
  type state = int

  (* represents values computed by evaluating type expression *)
  type value =
    | VConstant of Parsetree.constant
    | VTuple of value * value * value list
    | VFun of Parsetree.pattern * Parsetree.expression * environment
    (** functions (both recursive and non-recursive) with it's closure *)
    | VConstruct of string * value option
    (** [ Some 123, None, Ok (fun x -> x + 1) etc. ] *)
    | VPrimitive of string * (value -> (state, value, Error.t) M.t)
    (** [ print_value ] is presented as
        [ VPrimitive ("print_value", implementation) ] *)

  (** represents function closures (for both recursive and non-recursive functions) *)
  and environment = (string, value, Base.String.comparator_witness) Base.Map.t

  (** [ let pattern = value ] *)
  type value_binding = Parsetree.pattern * value

  type structure = value_binding * value_binding list

  let vprimitive name impl = VPrimitive (name, impl)

  open Base
  open Pprint
  open Pprint.Parens
  open Stdlib.Format

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
