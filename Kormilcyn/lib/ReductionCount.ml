open Lambda
open Ast
open Utils

type strat_count =
  { on_var : strat_count -> int -> name -> string t * int
  ; on_abs : strat_count -> int -> name -> string t -> string t * int
  ; on_app : strat_count -> int -> string t -> string t -> string t * int
  }

let apply_strat_count (st : strat_count) (count : int) (term : name t) : name t * int =
  if count <= 0
  then term, count
  else (
    match term with
    | Var name -> st.on_var st count name
    | Abs (x, b) -> st.on_abs st count x b
    | App (l, r) -> st.on_app st count l r)
;;

let without_strat_count =
  let on_var _st count name = var name, count in
  let on_abs _st count name e = abs name e, count in
  let on_app _st count l r = app l r, count in
  { on_var; on_abs; on_app }
;;

let cbn_strat_count =
  let on_app (st : strat_count) (count : int) (f : name t) (arg : name t) : name t * int =
    let f', count' = apply_strat_count st count f in
    match f' with
    | Abs (x, e) ->
      let count_after_beta = count' - 1 in
      let substituted_term = subst x ~by:arg e in
      apply_strat_count st count_after_beta substituted_term
    | f2 -> App (f2, arg), count'
  in
  { without_strat_count with on_app }
;;

let under_abstraction_count (st : strat_count) (count : int) (x : name) (b : name t)
  : name t * int
  =
  let e', new_count = apply_strat_count st count b in
  abs x e', new_count
;;

let cbv_strat_count =
  let on_app (st : strat_count) (count : int) (f : name t) (arg : name t) : name t * int =
    let f', count_after_f = apply_strat_count st count f in
    (* пошли вглубь функции, всё средуцировали, понизили count *)
    let arg', count_after_arg = apply_strat_count st count_after_f arg in
    (* пошли вглубь аргумента, всё средуцировали, понизили count *)
    match f' with
    | Abs (x, e) ->
      let count_after_beta = count_after_arg - 1 in
      let substituted = subst x ~by:arg' e in
      apply_strat_count st count_after_beta substituted
    | f2 -> App (f2, arg'), count_after_arg
  in
  { without_strat_count with on_app }
;;

let nor_strat_count =
  let on_app (st : strat_count) (count : int) (f : name t) (arg : name t) : name t * int =
    let f1, count_after_cbn = apply_strat_count cbn_strat_count count f in
    match f1 with
    | Abs (x, e) -> apply_strat_count st (count_after_cbn - 1) (subst x ~by:arg e)
    | f2 ->
      let f3, count_after_nor = apply_strat_count st count_after_cbn f2 in
      let arg2, count_after_arg = apply_strat_count st count_after_nor arg in
      App (f3, arg2), count_after_arg
  in
  { without_strat_count with on_app; on_abs = under_abstraction_count }
;;

let ao_strat_count = { cbv_strat_count with on_abs = under_abstraction_count }

let interpret_with_count st count term =
  let result_term, _ = apply_strat_count st count term in
  result_term
;;
