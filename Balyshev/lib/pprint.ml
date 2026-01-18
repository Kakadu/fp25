[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Stdlib.Format

module Parens = struct
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

  let set_parens ~(ctx : context) cases s =
    if List.exists cases ~f:(fun x -> Stdlib.compare x ctx = 0)
    then sprintf "(%s)" s
    else s
  ;;
end

let show_many ~sep show_item items =
  let pp_sep ppf () = pp_print_string ppf sep in
  let pp_item ppf item = pp_print_string ppf (show_item item) in
  asprintf "%a" (pp_print_list ~pp_sep pp_item) items
;;

let show_tuple show_item (a, b, xs) = show_many ~sep:", " show_item (a :: b :: xs)

let show_list_brackets show_item = function
  | [] -> "[]"
  | items -> sprintf "[ %s ]" (show_many ~sep:"; " show_item items)
;;

let show_list_cons =
  let open Parens in
  let aux ?(ctx = Free) (show_item : ?ctx:context -> 'a -> string) items =
    set_parens ~ctx [ App; Binop ]
    @@ String.concat ~sep:" :: " (List.map ~f:(show_item ~ctx:Binop) items)
  in
  aux
;;
