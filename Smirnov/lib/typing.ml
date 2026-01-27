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

let rec find_first_entry_in_context (v : string) (ctx : (string * mltype) list)
  : mltype option
  =
  match ctx with
  | (v1, tp) :: tail -> if v1 = v then Some tp else find_first_entry_in_context v tail
  | _ -> None
;;

let rec drop_first_entry_in_context (v : string) (ctx : (string * mltype) list) =
  match ctx with
  | (v1, _) :: tail when v1 = v -> tail
  | h :: tail -> h :: drop_first_entry_in_context v tail
  | [] -> []
;;

let rec rename_variable_raw qft v1 v2 =
  match qft with
  | Arrowtype (t1, t2) ->
    Arrowtype (rename_variable_raw t1 v1 v2, rename_variable_raw t2 v1 v2)
  | Vartype v when v = v1 -> Vartype v2
  | _ -> qft
;;

let rename_free_variable t v1 v2 =
  if List.mem v1 (snd t) then t else rename_variable_raw (fst t) v1 v2, snd t
;;

let instantiate t cnt =
  let helper acc v = rename_variable_raw (fst acc) v (snd acc), snd acc + 1 in
  let qftype, cnt = List.fold_left helper (fst t, cnt) (snd t) in
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
    let rec helper = function
      | h :: tail -> collect_vars (snd h) @ helper tail
      | [] -> []
    in
    List.sort_uniq compare (helper ctx)
  in
  let bound_vars =
    List.filter (fun x -> not (List.mem x (collect_vars_ctx ctx))) (collect_vars t)
  in
  List.fold_left
    (fun acc v ->
      ( (rename_variable_raw (fst (fst acc)) v (snd acc), snd acc :: snd (fst acc))
      , snd acc + 1 ))
    ((fst t, []), cnt)
    bound_vars
;;

let apply_subst subst t =
  let rec helper subst qft bound =
    match qft with
    | Vartype i ->
      (match List.find_opt (fun x -> fst x = i) subst with
       | Some (_, qft') -> if List.mem i bound then qft else qft'
       | None -> qft)
    | Arrowtype (t1, t2) -> Arrowtype (helper subst t1 bound, helper subst t2 bound)
    | _ -> qft
  in
  helper subst (fst t) (snd t), snd t
;;

let apply_subst_ctx subst ctx = List.map (fun x -> fst x, apply_subst subst (snd x)) ctx

let rec occurs i = function
  | Vartype j when i = j -> true
  | Arrowtype (t1, t2) -> occurs i t1 || occurs i t2
  | _ -> false
;;

let unify qft1 qft2 =
  let rec subst_var i t tp =
    match tp with
    | Arrowtype (t1, t2) -> Arrowtype (subst_var i t t1, subst_var i t t2)
    | Vartype j when i = j -> t
    | _ -> tp
  in
  (*  let pairs_to_string pairs = String.concat "\n" (List.map (fun x -> "[" ^ (type_to_string (fst x, [])) ^ " = " ^ (type_to_string (snd x, [])) ^ "]") pairs)
      in *)
  let rec helper pairs acc =
    (*    let () = Printf.printf "unify: %s\n%!" (pairs_to_string pairs) in *)
    match pairs with
    | (t1, t2) :: tail when t1 = t2 -> helper tail acc
    | (Vartype i, t2) :: tail ->
      if occurs i t2
      then None
      else
        Option.bind
          (helper
             (List.map (fun x -> subst_var i t2 (fst x), subst_var i t2 (snd x)) tail)
             (List.map (fun x -> fst x, subst_var i t2 (snd x)) acc))
          (fun x -> Some ((i, t2) :: x))
    | (t1, Vartype i) :: tail -> helper ((Vartype i, t1) :: tail) acc
    | (Arrowtype (t1, t2), Arrowtype (t3, t4)) :: tail ->
      helper ((t1, t3) :: (t2, t4) :: tail) acc
    | [] -> Some acc
    | _ -> None
  in
  helper [ qft1, qft2 ] []
;;

let hm_typechecker (term : mlterm) =
  let rec helper (term : mlterm) (ctx : (string * mltype) list) (cnt : int) =
    (*    let () = Printf.printf "%s\n%!" (mlterm_to_string term) in *)
    match term with
    | Var v ->
      (match find_first_entry_in_context v ctx with
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
      tp2, drop_first_entry_in_context v ctx'', cnt'''
    | LetRec (v, t1, t2) ->
      let tp1, ctx', cnt' = helper t1 ((v, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      (*      let _ = if occurs cnt (fst tp1) then raise (Failure "occurs check") else () in *)
      (*      let () = Printf.printf "let rec: %s : %s\n%!" (mlterm_to_string t1) (type_to_string tp1) in *)
      let tp1', cnt'' = quantify tp1 ctx' cnt' in
      let tp2, ctx'', cnt''' =
        helper t2 ((v, tp1') :: drop_first_entry_in_context v ctx') cnt''
      in
      tp2, drop_first_entry_in_context v ctx'', cnt'''
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
        match find_first_entry_in_context v ctx' with
        | Some value -> value
        | None -> raise (Failure "should not happen")
      in
      (*      let () = Printf.printf "Fun: v : %d, t : %s\n%!" cnt (type_to_string newtype) in *)
      (Arrowtype (fst newtype, fst t'), []), drop_first_entry_in_context v ctx', cnt'
  in
  helper term initial_context 0
;;
