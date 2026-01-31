(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type qf_mltype =
  | Basetype of string
  | Arrowtype of qf_mltype * qf_mltype
  | Vartype of int
  | Prod of qf_mltype * qf_mltype
  | Sum of qf_mltype * qf_mltype

type mltype = qf_mltype * int list

let type_to_string (t : mltype) =
  let rec helper = function
    | Basetype s -> s
    | Arrowtype (s1, s2) -> "(" ^ helper s1 ^ " -> " ^ helper s2 ^ ")"
    | Prod (s1, s2) -> "(" ^ helper s1 ^ " * " ^ helper s2 ^ ")"
    | Sum (s1, s2) -> "(" ^ helper s1 ^ " + " ^ helper s2 ^ ")"
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
  ; "&&", (Arrowtype (Basetype "bool", Arrowtype (Basetype "bool", Basetype "bool")), [])
  ; "||", (Arrowtype (Basetype "bool", Arrowtype (Basetype "bool", Basetype "bool")), [])
  ; "not", (Arrowtype (Basetype "bool", Basetype "bool"), [])
  ; "fst", (Arrowtype (Prod (Vartype 1, Vartype 2), Vartype 1), [ 1; 2 ])
  ; "snd", (Arrowtype (Prod (Vartype 1, Vartype 2), Vartype 2), [ 1; 2 ])
  ; "inl", (Arrowtype (Vartype 1, Sum (Vartype 1, Vartype 2)), [ 1; 2 ])
  ; "inr", (Arrowtype (Vartype 2, Sum (Vartype 1, Vartype 2)), [ 1; 2 ])
  ; "println_int", (Arrowtype (Basetype "int", Basetype "unit"), [])
  ; "raise", (Arrowtype (Basetype "exc", Vartype 1), [ 1 ])
  ]
;;

let rec subst_var (v : int) (t : qf_mltype) = function
  | Arrowtype (t1, t2) -> Arrowtype (subst_var v t t1, subst_var v t t2)
  | Prod (t1, t2) -> Prod (subst_var v t t1, subst_var v t t2)
  | Sum (t1, t2) -> Sum (subst_var v t t1, subst_var v t t2)
  | Vartype u when u = v -> t
  | _ as qft -> qft
;;

let instantiate (t : mltype) cnt : qf_mltype * int =
  let inst_var (t1, c) v = subst_var v (Vartype c) t1, c + 1 in
  let qftype, cnt = List.fold_left inst_var (fst t, cnt) (snd t) in
  (*   let () = Printf.printf "Inst: %s -> %s\n%!" (type_to_string t) (type_to_string (qftype, [])) in *)
  qftype, cnt
;;

let quantify (t : mltype) (ctx : (string * mltype) list) (cnt : int) =
  (*  let () = Printf.printf "Quantify %s\n%!" (type_to_string t) in *)
  assert (snd t = []);
  let collect_vars t =
    let rec helper = function
      | Vartype i -> [ i ]
      | Arrowtype (t1, t2) -> helper t1 @ helper t2
      | Prod (t1, t2) -> helper t1 @ helper t2
      | Sum (t1, t2) -> helper t1 @ helper t2
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
  | Prod (t1, t2) -> occurs i t1 || occurs i t2
  | Sum (t1, t2) -> occurs i t1 || occurs i t2
  | _ -> false
;;

let unify pairs =
  let rec helper pairs acc =
    match pairs with
    | (t1, t2) :: tail when t1 = t2 -> helper tail acc
    | (Vartype i, t2) :: tail ->
      if occurs i t2
      then None
      else
        helper
          (List.map (fun (u1, u2) -> subst_var i t2 u1, subst_var i t2 u2) tail)
          ((i, t2) :: List.map (fun (j, t) -> j, subst_var i t2 t) acc)
    | (t1, Vartype i) :: tail -> helper ((Vartype i, t1) :: tail) acc
    | (Arrowtype (t1, t2), Arrowtype (t3, t4)) :: tail ->
      helper ((t1, t3) :: (t2, t4) :: tail) acc
    | (Prod (t1, t2), Prod (t3, t4)) :: tail -> helper ((t1, t3) :: (t2, t4) :: tail) acc
    | (Sum (t1, t2), Sum (t3, t4)) :: tail -> helper ((t1, t3) :: (t2, t4) :: tail) acc
    | [] -> Some acc
    | _ -> None
  in
  helper pairs []
;;

(* There are lots of HM inference algorithm. Wikipedia presents J and S variants. *)
(* I failed to implement Kosarev's variant of HM, implemented Transpodis' algo instead. *)
(* Its complexity is DEXPTIME but the algo is much easier to understand *)
(* I failed in Transpodis' too :-( So implement a naive algo, also DEXPTIME *)
let hm_typechecker (term : mlterm) : mltype =
  let rec helper (term : mlterm) (ctx : (string * mltype) list) (cnt : int)
    : int * (qf_mltype * qf_mltype) list * (string * mltype) list * int
    =
    match term with
    | Var v ->
      (match List.assoc_opt v ctx with
       | Some tp ->
         let t, cnt = instantiate tp cnt in
         cnt, [ Vartype cnt, t ], ctx, cnt + 1
       | _ -> raise (Failure "unbound variable"))
    | Int _ -> cnt, [ Vartype cnt, Basetype "int" ], ctx, cnt + 1
    | Bool _ -> cnt, [ Vartype cnt, Basetype "bool" ], ctx, cnt + 1
    | Unit -> cnt, [ Vartype cnt, Basetype "unit" ], ctx, cnt + 1
    | ITE (t1, t2, t3) ->
      let var1, eqs1, ctx, cnt = helper t1 ctx cnt in
      let var2, eqs2, ctx, cnt = helper t2 ctx cnt in
      let var3, eqs3, ctx, cnt = helper t3 ctx cnt in
      let neweqs = [ Vartype var2, Vartype var3; Vartype var1, Basetype "bool" ] in
      var2, neweqs @ eqs1 @ eqs2 @ eqs3, ctx, cnt
    | Let (v, t1, t2) ->
      let var1, eqs1, ctx, cnt = helper t1 ctx cnt in
      (match unify eqs1 with
       | Some subst ->
         let ctx = apply_subst_ctx subst ctx in
         let tp1 = apply_subst subst (Vartype var1, []) in
         let tp1, cnt = quantify tp1 ctx cnt in
         let var2, eqs2, ctx, cnt = helper t2 ((v, tp1) :: ctx) cnt in
         var2, eqs2, List.remove_assoc v ctx, cnt
       | None -> raise (Failure "let unification failed"))
    | LetRec (v, t1, t2) ->
      let var1, eqs1, ctx, cnt' = helper t1 ((v, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      (match unify ((Vartype var1, Vartype cnt) :: eqs1) with
       | Some subst ->
         let ctx = apply_subst_ctx subst ctx in
         let tp1 = apply_subst subst (Vartype var1, []) in
         let tp1, cnt = quantify tp1 ctx cnt' in
         let var2, eqs2, ctx, cnt = helper t2 ((v, tp1) :: ctx) cnt in
         var2, eqs2, List.remove_assoc v ctx, cnt
       | None -> raise (Failure "letrec unification failed"))
    | LetExc (e, t) ->
      let var, eqs, ctx, cnt = helper t ((e, (Basetype "exc", [])) :: ctx) (cnt + 1) in
      var, eqs, List.remove_assoc e ctx, cnt
    | App (t1, t2) ->
      let var1, eqs1, ctx, cnt = helper t1 ctx cnt in
      let var2, eqs2, ctx, cnt = helper t2 ctx cnt in
      let neweqs = [ Vartype var1, Arrowtype (Vartype var2, Vartype cnt) ] in
      cnt, neweqs @ eqs1 @ eqs2, ctx, cnt + 1
    | Fun (v, t) ->
      let var, eqs, ctx, cnt' = helper t ((v, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      let neweqs = [ Vartype cnt', Arrowtype (Vartype cnt, Vartype var) ] in
      cnt', neweqs @ eqs, List.remove_assoc v ctx, cnt' + 1
    | Pair (t1, t2) ->
      let var1, eqs1, ctx, cnt = helper t1 ctx cnt in
      let var2, eqs2, ctx, cnt = helper t2 ctx cnt in
      cnt, ((Vartype cnt, Prod (Vartype var1, Vartype var2)) :: eqs1) @ eqs2, ctx, cnt + 1
    | Match (t, v1, t1, v2, t2) ->
      let var, eqs, ctx, cnt = helper t ctx cnt in
      let var1, eqs1, ctx, cnt1 = helper t1 ((v1, (Vartype cnt, [])) :: ctx) (cnt + 1) in
      let var2, eqs2, ctx, cnt2 =
        helper t2 ((v2, (Vartype cnt1, [])) :: List.remove_assoc v1 ctx) (cnt1 + 1)
      in
      let neweqs =
        [ Vartype var1, Vartype var2; Vartype var, Sum (Vartype cnt, Vartype cnt1) ]
      in
      var1, neweqs @ eqs @ eqs1 @ eqs2, ctx, cnt2
    | Try (t, l) ->
      let (var : int), eqs, ctx, cnt = helper t ctx cnt in
      let acc0 = [], eqs, ctx, cnt in
      let (varl : int list), eqs1, ctx, cnt =
        List.fold_left
          (fun (varl, eqs, ctx, cnt) (_, t) ->
            let (var : int), eqs1, ctx, cnt = helper t ctx cnt in
            var :: varl, eqs @ eqs1, ctx, cnt)
          acc0
          l
      in
      var, List.map (fun v -> Vartype var, Vartype v) varl @ eqs @ eqs1, ctx, cnt
  in
  let var, eqs, _, _ = helper term initial_context 0 in
  match unify eqs with
  | Some subst -> apply_subst subst (Vartype var, [])
  | None -> raise (Failure "final unification failed")
;;
