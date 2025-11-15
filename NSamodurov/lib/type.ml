type ty =
  | TGround of string
  | TArrow of ty * ty
  | TVar of int (* | TTuple of ty * ty * ty list *)
[@@deriving show { with_path = false }]

let tground s = TGround s
let tarrow a b = TArrow (a, b)
let tvar b = TVar b
let tint = tground "int"

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

  (* let pp ppf s = *)
  (*   Format.printf "<. "; *)
  (*   iter (fun x -> Format.fprintf ppf "%d" x) s; *)
  (*   Format.printf ".> " *)
  (* ;; *)
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

type binder_set = ISet.t
type scheme = binder_set * ty
