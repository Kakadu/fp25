open Ast

let list_remove x = Base.List.filter ~f:(fun a -> not (String.equal a x))

let free_vars =
  let rec helper acc = function
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
  in
  helper []
;;

let is_free_in x term = Base.List.mem (free_vars term) x ~equal:String.equal
