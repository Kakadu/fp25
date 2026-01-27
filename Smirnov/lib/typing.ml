(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type qf_mltype =
  | Basetype of string
  | Arrowtype of qf_mltype * qf_mltype
  | Vartype of int

type mltype = qf_mltype * int list

let type_to_string (t : mltype) =
  let rec helper = function
    | Basetype s -> s
    | Arrowtype (s1, s2) -> "(" ^ helper s1 ^ " -> " ^ helper s2 ^ ")"
    | Vartype i -> "bv" ^ string_of_int i
  in
  helper (fst t)
;;

let initial_context : (string * mltype) list =
  [ "+", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "int")), [])
  ; "-", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "int")), [])
  ; "*", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "int")), [])
  ; "==", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "bool")), [])
  ; "<", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "bool")), [])
  ; ">", (Arrowtype (Basetype "int", Arrowtype (Basetype "int", Basetype "bool")), [])
  ; "println_int", (Arrowtype (Basetype "int", Basetype "unit"), [])
  ]
;;

let rec subst_var (v : int) (t : qf_mltype) = function
  | Arrowtype (t1, t2) -> Arrowtype (subst_var v t t1, subst_var v t t2)
  | Vartype u when u = v -> t
  | _ as qft -> qft
;;

let instantiate (t : mltype) cnt =
  let inst_var (t1, c) v = subst_var v (Vartype c) t1, c + 1 in
  let qftype, cnt = List.fold_left inst_var (fst t, cnt) (snd t) in
  (*   let () = Printf.printf "Inst: %s -> %s\n%!" (type_to_string t) (type_to_string (qftype, [])) in *)
  (qftype, []), cnt
;;

let quantify (t : mltype) (ctx : (string * mltype) list) (cnt : int) =
  (*  let () = Printf.printf "Quantify %s\n%!" (type_to_string t) in *)
  assert (snd t = []);
  let collect_vars t =
    let rec helper = function
      | Vartype i -> [ i ]
      | Arrowtype (t1, t2) -> helper t1 @ helper t2
      | _ -> []
    in
    List.filter
      (fun x -> not (List.mem x (snd t)))
      (List.sort_uniq compare (helper (fst t)))
  in
  let collect_vars_ctx ctx =
    ctx |> List.concat_map (fun x -> collect_vars (snd x)) |> List.sort_uniq compare
  in
  let bound_vars =
    List.filter (fun x -> not (List.mem x (collect_vars_ctx ctx))) (collect_vars t)
  in
  List.fold_left
    (fun ((t, vs), c) v -> (subst_var v (Vartype c) t, c :: vs), c + 1)
    ((fst t, []), cnt)
    bound_vars
;;

let apply_subst subst ((t, bound) : mltype) =
  let subst' = List.filter (fun (v, _) -> not (List.mem v bound)) subst in
  let helper acc (v, t1) = subst_var v t1 acc in
  List.fold_left helper t subst', bound
;;

let apply_subst_ctx subst ctx = List.map (fun (v, t) -> v, apply_subst subst t) ctx

let rec occurs i = function
  | Vartype j when i = j -> true
  | Arrowtype (t1, t2) -> occurs i t1 || occurs i t2
  | _ -> false
;;

let unify qft1 qft2 =
  let rec helper pairs acc =
    match pairs with
    | (t1, t2) :: tail when t1 = t2 -> helper tail acc
    | (Vartype i, t2) :: tail ->
      if occurs i t2
      then None
      else
        Option.bind
          (helper
             (List.map (fun (u1, u2) -> subst_var i t2 u1, subst_var i t2 u2) tail)
             (List.map (fun (j, t) -> j, subst_var i t2 t) acc))
          (fun x -> Some ((i, t2) :: x))
    | (t1, Vartype i) :: tail -> helper ((Vartype i, t1) :: tail) acc
    | (Arrowtype (t1, t2), Arrowtype (t3, t4)) :: tail ->
      helper ((t1, t3) :: (t2, t4) :: tail) acc
    | [] -> Some acc
    | _ -> None
  in
  helper [ qft1, qft2 ] []
;;

(* There are lots of HM inference algorithm. Wikipedia presents J and S variants. *)
(* I failed to implement Kosarev's variant of HM, implemented Transpodis' algo instead. *)
(* Its complexity is DEXPTIME but the algo is much easier to understand *)
let hm_typechecker (term : mlterm) =
  let rec helper (term : mlterm) (ctx : (string * mltype) list) (cnt : int) =
    (*    let () = Printf.printf "%s\n%!" (mlterm_to_string term) in *)
    match term with
    | Var v ->
      (match List.assoc_opt v ctx with
       | Some tp ->
         let t', cnt' = instantiate tp cnt in
         t', ctx, cnt'
       | _ -> raise (Failure "unbound variable"))
    | Int _ -> (Basetype "int", []), ctx, cnt
    | Bool _ -> (Basetype "bool", []), ctx, cnt
    | Unit -> (Basetype "unit", []), ctx, cnt
    | ITE (t1, t2, t3) ->
      let tp1, ctx', cnt' = helper t1 ctx cnt in
      let tp2, ctx'', cnt'' = helper t2 ctx' cnt' in
      let tp3, ctx''', cnt''' = helper t3 ctx'' cnt'' in
      if tp1 <> (Basetype "bool", [])
      then raise (Failure "condition is not bool")
      else (
        match unify (fst tp2) (fst tp3) with
        | Some subst -> apply_subst subst tp2, apply_subst_ctx subst ctx''', cnt'''
        | None -> raise (Failure "then and else diff types"))
    | Let (v, t1, t2) ->
      let tp1, (ctx' : (string * mltype) list), (cnt' : int) = helper t1 ctx cnt in
      (*      let () = Printf.printf "let: %s : %s\n%!" (mlterm_to_string t1) (type_to_string tp1) in *)
      let tp1', cnt'' = quantify tp1 ctx' cnt' in
      let tp2, ctx'', cnt''' = helper t2 ((v, tp1') :: ctx') cnt'' in
      tp2, List.remove_assoc v ctx'', cnt'''
    | LetRec (v, t1, t2) ->
      let tp1, ctx', cnt' = helper t1 ((v, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      (*      let _ = if occurs cnt (fst tp1) then raise (Failure "occurs check") else () in *)
      (*      let () = Printf.printf "let rec: %s : %s\n%!" (mlterm_to_string t1) (type_to_string tp1) in *)
      let tp1', cnt'' = quantify tp1 ctx' cnt' in
      let tp2, ctx'', cnt''' = helper t2 ((v, tp1') :: List.remove_assoc v ctx') cnt'' in
      tp2, List.remove_assoc v ctx'', cnt'''
    | App (t1, t2) ->
      let tp1, ctx', cnt' = helper t1 ctx cnt in
      let tp2, ctx'', cnt'' = helper t2 ctx' cnt' in
      (*      let () = Printf.printf "app: %s and %s\n%!" (type_to_string tp1) (type_to_string (Arrowtype (fst tp2, Vartype cnt''), [])) in *)
      (match unify (fst tp1) (Arrowtype (fst tp2, Vartype cnt'')) with
       | Some subst ->
         apply_subst subst (Vartype cnt'', []), apply_subst_ctx subst ctx'', cnt'' + 1
       | None -> raise (Failure "function and args types mismatch"))
    | Fun (v, t) ->
      let t', ctx', cnt' = helper t ((v, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      let newtype =
        match List.assoc_opt v ctx' with
        | Some value -> value
        | None -> raise (Failure "should not happen")
      in
      (*      let () = Printf.printf "Fun: v : %d, t : %s\n%!" cnt (type_to_string newtype) in *)
      (Arrowtype (fst newtype, fst t'), []), List.remove_assoc v ctx', cnt'
  in
  helper term initial_context 0
;;
