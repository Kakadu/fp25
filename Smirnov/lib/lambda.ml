(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type lterm =
  | Var of string
  | Int of int
  | Unit
  | App of lterm * lterm
  | Abs of string * lterm

let ltrue = Abs ("x", Abs ("y", Var "x"))
let lfalse = Abs ("x", Abs ("y", Var "y"))

let fix_comb_no =
  let helper = Abs ("x", App (Var "f", App (Var "x", Var "x"))) in
  Abs ("f", App (helper, helper))
;;

let fix_comb_cbv =
  let helper =
    Abs ("x", App (Var "f", Abs ("v", App (App (Var "x", Var "x"), Var "v"))))
  in
  Abs ("f", App (helper, helper))
;;

let fix_comb_cbn = fix_comb_no

let try_stdlib = function
  | App (Var "println_int", Int i) ->
    let () = Printf.printf "%d\n%!" i in
    Some Unit
  | App (App (Var "+", Int x), Int y) -> Some (Int (x + y))
  | App (App (Var "-", Int x), Int y) -> Some (Int (x - y))
  | App (App (Var "*", Int x), Int y) -> Some (Int (x * y))
  | App (App (Var "==", Int x), Int y) -> Some (if x = y then ltrue else lfalse)
  | App (App (Var ">", Int x), Int y) -> Some (if x > y then ltrue else lfalse)
  | App (App (Var "<", Int x), Int y) -> Some (if x < y then ltrue else lfalse)
  | _ -> None
;;

let fresh_var vars =
  let cands = List.init (List.length vars + 1) (fun i -> "fv" ^ string_of_int i) in
  let res = List.filter (fun x -> not (List.mem x vars)) cands in
  match res with
  | h :: _ -> h
  | [] -> raise (Failure "Should not happen!")
;;

let rec lterm_to_string = function
  | Var x -> x
  | Int i -> Printf.sprintf "%d" i
  | Unit -> "()"
  | App (t1, t2) -> "(" ^ lterm_to_string t1 ^ " " ^ lterm_to_string t2 ^ ")"
  | Abs (x, t2) -> "(\\" ^ x ^ "." ^ lterm_to_string t2 ^ ")"
;;

let rec lterm_to_string_typed term tp =
  assert (snd tp = []);
  match term, fst tp with
  | Var x, _ -> x
  | Int i, Typing.Basetype "int" -> Printf.sprintf "%d" i
  | Unit, Typing.Basetype "unit" -> "()"
  | Abs (x, App (App (Var y, t1), t2)), Typing.Pair (tp1, tp2) when x = y ->
    "("
    ^ lterm_to_string_typed t1 (tp1, [])
    ^ ", "
    ^ lterm_to_string_typed t2 (tp2, [])
    ^ ")"
  | Abs (x, Abs (_, Var z)), Typing.Basetype "bool" when z = x -> "true"
  | Abs (_, Abs (y, Var z)), Typing.Basetype "bool" when z = y -> "false"
  | Abs (_, _), _ -> "Fun"
  | _, _ -> raise (Failure "Should not happen!")
;;

let vars t =
  let rec helper = function
    | Var x -> [ x ]
    | App (t1, t2) -> helper t1 @ helper t2
    | Abs (x, t2) -> [ x ] @ helper t2
    | _ -> []
  in
  List.sort_uniq compare (helper t)
;;

let fvars t =
  let rec helper = function
    | Var x -> [ x ]
    | App (t1, t2) -> helper t1 @ helper t2
    | Abs (x, t2) -> List.filter (fun y -> x <> y) (helper t2)
    | _ -> []
  in
  List.sort_uniq compare (helper t)
;;

let rec rename_free_var term x y =
  match term with
  | Var u when u = x -> Var y
  | App (t1, t2) -> App (rename_free_var t1 x y, rename_free_var t2 x y)
  | Abs (u, _) when u = x -> term
  | Abs (u, t) -> Abs (u, rename_free_var t x y)
  | _ -> term
;;

(* term[x |-> t] *)
let subst term x t =
  let rec helper term x t vars =
    match term with
    | Var y -> if x = y then t, vars else term, vars
    | Int _ -> term, vars
    | Unit -> term, vars
    | App (t1, t2) ->
      let r1 = helper t1 x t vars in
      let r2 = helper t2 x t (snd r1) in
      App (fst r1, fst r2), snd r2
    | Abs (y, _) when y = x -> term, vars
    | Abs (y, t2) ->
      (match List.find_opt (( = ) y) (fvars t) with
       | Some _ ->
         let u = fresh_var vars in
         let r = helper (rename_free_var t2 y u) x t ([ u ] @ vars) in
         Abs (u, fst r), snd r
       | None ->
         let r = helper t2 x t vars in
         Abs (y, fst r), snd r)
  in
  fst (helper term x t ([ x ] @ vars term @ vars t))
;;

let rec beta_step_in_cbv term =
  match try_stdlib term with
  | Some t -> Some t
  | None ->
    (match term with
     | Abs (_, _) -> None
     | App (Abs (x, y), z) ->
       (match beta_step_in_cbv z with
        | Some t -> Some (App (Abs (x, y), t))
        | None -> Some (subst y x z))
     | App (t1, t2) ->
       (match beta_step_in_cbv t1 with
        | Some t -> Some (App (t, t2))
        | None ->
          (match beta_step_in_cbv t2 with
           | Some t -> Some (App (t1, t))
           | None -> None))
     | _ -> None)
;;

let rec beta_reduce strategy x =
  (* let () = Printf.printf "%s\n" (lterm_to_string x) in *)
  match strategy x with
  | Some y -> beta_reduce strategy y
  | None -> x
;;
