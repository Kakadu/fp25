[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type ty =
  | TGround of string (** Primitive type [int, bool, float] *)
  | TArrow of ty * ty (** Type of abstraction [t1 -> t2]  *)
  | TVar of int (** Variable [v]  *)
    (* | TTuple of ty * ty * ty list (\** Ordered sequence [(e1, e2, ..., en)] *\) *)
[@@deriving show { with_path = false }]

let tground s = TGround s
let tarrow a b = TArrow (a, b)
let tvar b = TVar b
let tint = tground "int"
let tbool = tground "bool"

module ISet = struct
  include Set.Make (Int)

  let pp ppf s =
    Format.printf "<. ";
    iter (Format.fprintf ppf "%d") s;
    Format.printf ".> "
  ;;
end
[@@deriving show { with_path = false }]

module IMap = struct
  include Map.Make (Int)

  let pp ppf s =
    Format.printf "<. ";
    iter (fun _ -> Format.fprintf ppf "%d") s;
    Format.printf ".> "
  ;;
end
[@@deriving show { with_path = false }]

let fv =
  let rec helper set = function
    | TVar b -> ISet.add b set
    | TArrow (e1, e2) -> helper (helper set e1) e2
    | TGround _ -> set
  in
  helper ISet.empty
;;

let rec occurs_in t = function
  | TGround _ -> false
  | TArrow (e1, e2) -> occurs_in t e1 || occurs_in t e2
  | TVar _ as b -> b = t
;;

type binder_set = ISet.t (* A set of type variables under quatifier *)
type scheme = binder_set * ty (* Used for let polymorphism *)
