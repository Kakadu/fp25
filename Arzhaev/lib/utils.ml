module type TABLE = sig
  type 'a t

  val empty : 'a t
  val extend : string -> 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a option
  val contains_value : 'a -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Table : TABLE = struct
  module StringMap = Map.Make (String)

  type 'a t = 'a StringMap.t

  let empty = StringMap.empty
  let lookup x env = StringMap.find_opt x env
  let extend x ty env = StringMap.add x ty env

  let contains_value target_value map =
    StringMap.fold (fun _key value acc -> acc || value = target_value) map false
  ;;

  let pp pp_value fmt env =
    let bindings = StringMap.bindings env in
    match bindings with
    | [] -> Format.fprintf fmt "{}"
    | _ ->
      Format.fprintf fmt "{ ";
      List.iter (fun (k, v) -> Format.fprintf fmt "%s -> %a\n" k pp_value v) bindings;
      Format.fprintf fmt " }"
  ;;
end

module type SET_WITH_TO_LIST = sig
  include Set.S

  val to_list : t -> elt list
end

module SetWithToList (S : Set.S) : SET_WITH_TO_LIST with type elt = S.elt = struct
  include S

  let to_list s = S.fold (fun elt acc -> elt :: acc) s []
end

let rec charlst_to_str = function
  | h :: tl -> String.make 1 h ^ charlst_to_str tl
  | [] -> ""
;;

let str_to_charlst s =
  let rec helper i l = if i < 0 then l else helper (i - 1) (s.[i] :: l) in
  helper (String.length s - 1) []
;;

let get_next_letter c =
  match c with
  | 'a' .. 'y' -> Char.chr (Char.code c + 1)
  | _ -> 'a'
;;
