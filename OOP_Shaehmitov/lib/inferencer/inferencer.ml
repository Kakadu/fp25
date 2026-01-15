[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base
module List = Stdlib.List
module Map = Stdlib.Map
module String = Stdlib.String
module Option = Stdlib.Option
module Format = Stdlib.Format

module TypeChecker = struct
  type error =
    | UnboundVariable of string
    | UnificationError of string * string
    | ClassNotFound of string
    | MethodNotFound of string
    | InfiniteType of string
    | ArgumentCountMismatch of string * int * int
    | ConstructorError of string
    | Other of string

  type 'a res = ('a, error) Result.t

  let return x = Ok x
  let fail e = Error e

  let ( let* ) res f =
    match res with
    | Ok v -> f v
    | Error e -> Error e
  ;;

  type u_typ =
    | TVar of int
    | TInt
    | TBool
    | TUnit
    | TFun of u_typ * u_typ
    | TTuple of u_typ list
    | TClass of string

  type method_constraint =
    { name : string
    ; args : u_typ list
    ; ret : u_typ
    }

  module IntMap = Map.Make (Int)
  module StrMap = Map.Make (String)
  module EnvMap = StrMap

  type subst = u_typ IntMap.t
  type scheme = Scheme of int list * u_typ * (int * method_constraint list) list

  let rec pp_type fmt = function
    | TVar i -> Format.fprintf fmt "'_a%d" i
    | TInt -> Format.fprintf fmt "int"
    | TBool -> Format.fprintf fmt "bool"
    | TUnit -> Format.fprintf fmt "unit"
    | TFun (t1, t2) ->
      let pp_lhs fmt t =
        match t with
        | TFun _ -> Format.fprintf fmt "(%a)" pp_type t
        | _ -> pp_type fmt t
      in
      Format.fprintf fmt "%a -> %a" pp_lhs t1 pp_type t2
    | TTuple ts ->
      let pp_elem fmt t =
        match t with
        | TFun _ -> Format.fprintf fmt "(%a)" pp_type t
        | _ -> pp_type fmt t
      in
      Format.fprintf fmt "(";
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ") pp_elem fmt ts;
      Format.fprintf fmt ")"
    | TClass n -> Format.fprintf fmt "%s" n
  ;;

  let pp_method_constraint fmt c =
    Format.fprintf
      fmt
      "%s %a: %a"
      c.name
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_type)
      c.args
      pp_type
      c.ret
  ;;

  let pp_scheme fmt (Scheme (vars, t, cs)) =
    if Base.List.is_empty vars && Base.List.is_empty cs
    then pp_type fmt t
    else (
      Format.fprintf fmt "forall ... %a" pp_type t;
      if not (Base.List.is_empty cs)
      then (
        Format.fprintf fmt " where ";
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt " and ")
          (fun fmt (id, mcs) ->
            Format.fprintf
              fmt
              "'_a%d has %a"
              id
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                 pp_method_constraint)
              mcs)
          fmt
          cs))
  ;;

  let pp_error fmt = function
    | UnboundVariable s -> Format.fprintf fmt "Unbound: %s" s
    | UnificationError (a, b) -> Format.fprintf fmt "Unify: %s vs %s" a b
    | ClassNotFound c -> Format.fprintf fmt "No class %s" c
    | MethodNotFound m -> Format.fprintf fmt "No method %s" m
    | ConstructorError s -> Format.fprintf fmt "New error: %s" s
    | ArgumentCountMismatch (s, e, g) ->
      Format.fprintf fmt "Arg count mismatch %s: %d vs %d" s e g
    | Other s -> Format.fprintf fmt "%s" s
    | InfiniteType _ -> Format.fprintf fmt "Occurs check failed for type variable"
  ;;

  let rec apply_subst (s : subst) (t : u_typ) =
    match t with
    | TVar id ->
      (match IntMap.find_opt id s with
       | Some t' -> apply_subst s t'
       | None -> t)
    | TFun (t1, t2) -> TFun (apply_subst s t1, apply_subst s t2)
    | TTuple ts -> TTuple (List.map (apply_subst s) ts)
    | _ -> t
  ;;

  type class_info =
    { parent : string option
    ; params : u_typ list
    ; methods_sch : scheme StrMap.t
    ; fields : scheme StrMap.t
    }

  type class_table = class_info StrMap.t

  type check_state =
    { subst : subst
    ; constraints : method_constraint list IntMap.t
    ; next_id : int
    ; ct : class_table
    }

  let initial_state ct =
    { subst = IntMap.empty; constraints = IntMap.empty; next_id = 0; ct }
  ;;

  let fresh_var state =
    let id = state.next_id + 1 in
    TVar id, { state with next_id = id }
  ;;

  let apply_subst_to_constraint s c =
    { c with args = List.map (apply_subst s) c.args; ret = apply_subst s c.ret }
  ;;

  let rec occurs_in vid = function
    | TVar i -> i = vid
    | TFun (a, b) -> occurs_in vid a || occurs_in vid b
    | TTuple ts -> List.exists (occurs_in vid) ts
    | _ -> false
  ;;

  let rec find_method_scheme ct cls m =
    match StrMap.find_opt cls ct with
    | Some i ->
      (match StrMap.find_opt m i.methods_sch with
       | Some s -> Some s
       | None -> Option.bind i.parent (fun p -> find_method_scheme ct p m))
    | None -> None
  ;;

  let rec is_subclass ct child parent =
    String.equal child parent
    ||
    match StrMap.find_opt child ct with
    | Some info ->
      (match info.parent with
       | Some p -> is_subclass ct p parent
       | None -> false)
    | None -> false
  ;;

  let rec get_inheritance_chain ct cls =
    match StrMap.find_opt cls ct with
    | None -> [ cls ]
    | Some info ->
      cls
      ::
      (match info.parent with
       | Some p -> get_inheritance_chain ct p
       | None -> [])
  ;;

  let get_lcs ct c1 c2 =
    let chain1 = get_inheritance_chain ct c1 in
    let chain2 = get_inheritance_chain ct c2 in
    List.find_opt (fun x -> List.exists (String.equal x) chain2) chain1
  ;;

  let instantiate (Scheme (bound, body, constrs)) state =
    let s_map, st =
      List.fold_left
        (fun (m, s) id ->
          let v, s' = fresh_var s in
          IntMap.add id v m, s')
        (IntMap.empty, state)
        bound
    in
    let map_id id =
      match IntMap.find_opt id s_map with
      | Some (TVar i) -> i
      | _ -> id
    in
    let rep t = apply_subst s_map t in
    let r_constrs =
      List.fold_left
        (fun m (id, cs) ->
          IntMap.add
            (map_id id)
            (List.map
               (fun c -> { c with args = List.map rep c.args; ret = rep c.ret })
               cs)
            m)
        st.constraints
        constrs
    in
    let rec final_rep t =
      match t with
      | TVar i -> Option.value (IntMap.find_opt i s_map) ~default:t
      | TFun (a, b) -> TFun (final_rep a, final_rep b)
      | TTuple ts -> TTuple (List.map final_rep ts)
      | _ -> t
    in
    final_rep body, { st with constraints = r_constrs }
  ;;

  let rec unify state t1 t2 =
    let ct = state.ct in
    let t1', t2' = apply_subst state.subst t1, apply_subst state.subst t2 in
    let solve_var id t st =
      if occurs_in id t
      then fail (InfiniteType "occurs")
      else (
        let st_sub = { st with subst = IntMap.add id t st.subst } in
        let old_cs = Option.value (IntMap.find_opt id st.constraints) ~default:[] in
        match t with
        | TClass c -> verify_constraints st_sub c old_cs
        | TVar oid ->
          let ex = Option.value (IntMap.find_opt oid st.constraints) ~default:[] in
          return { st_sub with constraints = IntMap.add oid (old_cs @ ex) st.constraints }
        | _ ->
          if List.length old_cs > 0
          then
            fail (UnificationError ("constraint on prim", Format.asprintf "%a" pp_type t))
          else return st_sub)
    in
    match t1', t2' with
    | TVar i1, TVar i2 when i1 = i2 -> return state
    | TVar id, t -> solve_var id t state
    | t, TVar id -> solve_var id t state
    | TInt, TInt -> return state
    | TBool, TBool -> return state
    | TUnit, TUnit -> return state
    | TClass a, TClass b ->
      if is_subclass ct a b || is_subclass ct b a
      then return state
      else
        fail
          (UnificationError
             (Format.asprintf "%a" pp_type t1', Format.asprintf "%a" pp_type t2'))
    | TFun (a1, b1), TFun (a2, b2) ->
      let* s = unify state a1 a2 in
      unify s b1 b2
    | TTuple a, TTuple b ->
      if List.length a <> List.length b
      then fail (UnificationError ("tup len", ""))
      else
        List.fold_left2
          (fun r x y ->
            let* s = r in
            unify s x y)
          (return state)
          a
          b
    | _ ->
      fail
        (UnificationError
           (Format.asprintf "%a" pp_type t1', Format.asprintf "%a" pp_type t2'))

  and verify_constraints state cls cs =
    let ct = state.ct in
    List.fold_left
      (fun acc c ->
        let* s = acc in
        match find_method_scheme ct cls c.name with
        | None -> fail (MethodNotFound c.name)
        | Some sc ->
          let mt, s1 = instantiate sc s in
          let ex = List.fold_right (fun a r -> TFun (a, r)) c.args c.ret in
          unify s1 mt ex)
      (return state)
      cs
  ;;

  type env = scheme StrMap.t

  let rec free_vars = function
    | TVar i -> [ i ]
    | TFun (a, b) -> free_vars a @ free_vars b
    | TTuple ts -> List.concat_map free_vars ts
    | _ -> []
  ;;

  let free_vars_env env =
    StrMap.fold
      (fun _ (Scheme (b, body, _)) a ->
        List.filter (fun v -> not (List.mem v b)) (free_vars body) @ a)
      env
      []
  ;;

  let generalize env state t =
    let t_norm = apply_subst state.subst t in
    let b_vars =
      List.filter
        (fun v -> not (List.mem v (free_vars_env env)))
        (List.sort_uniq Int.compare (free_vars t_norm))
    in
    let b_cs =
      List.filter_map
        (fun v ->
          Option.map
            (fun cs -> v, List.map (apply_subst_to_constraint state.subst) cs)
            (IntMap.find_opt v state.constraints))
        b_vars
    in
    Scheme (b_vars, t_norm, b_cs)
  ;;

  let lookup_var env n s =
    match StrMap.find_opt n env with
    | Some sch -> return (instantiate sch s)
    | None -> fail (UnboundVariable n)
  ;;

  let rec infer env state = function
    | Const (CInt _) -> return (TInt, state)
    | Const (CBool _) -> return (TBool, state)
    | Const CUnit -> return (TUnit, state)
    | Var n -> lookup_var env n state
    | UnOp (op, e) ->
      let* t, s = infer env state e in
      (match op with
       | Neg ->
         let* s2 = unify s t TInt in
         return (TInt, s2)
       | Not ->
         let* s2 = unify s t TBool in
         return (TBool, s2))
    | BinOp (op, a, b) ->
      let* t1, s1 = infer env state a in
      let* t2, s2 = infer env s1 b in
      let chk ex rt st =
        let* s' = unify st t1 ex in
        let* s'' = unify s' t2 ex in
        return (rt, s'')
      in
      (match op with
       | Add -> chk TInt TInt s2
       | Sub -> chk TInt TInt s2
       | Mul -> chk TInt TInt s2
       | Div -> chk TInt TInt s2
       | Lt -> chk TInt TBool s2
       | Le -> chk TInt TBool s2
       | Gt -> chk TInt TBool s2
       | Ge -> chk TInt TBool s2
       | And -> chk TBool TBool s2
       | Or -> chk TBool TBool s2
       | Eq -> chk TInt TBool s2
       | Neq -> chk TInt TBool s2)
    | If (c, t, f) ->
      let* tc, s1 = infer env state c in
      let* s2 = unify s1 tc TBool in
      let* tt, s3 = infer env s2 t in
      let* tf, s4 = infer env s3 f in
      let tt_norm = apply_subst s4.subst tt in
      let tf_norm = apply_subst s4.subst tf in
      (match tt_norm, tf_norm with
       | TClass c1, TClass c2 ->
         (match get_lcs s4.ct c1 c2 with
          | Some lcs -> return (TClass lcs, s4)
          | None -> fail (UnificationError (c1, c2)))
       | _ ->
         let* s5 = unify s4 tt tf in
         return (tt, s5))
    | Tuple es ->
      let rec loop s acc = function
        | [] -> return (TTuple (List.rev acc), s)
        | e :: r ->
          let* t, s' = infer env s e in
          loop s' (t :: acc) r
      in
      loop state [] es
    | App (fn, arg) ->
      let* tf, s1 = infer env state fn in
      let* ta, s2 = infer env s1 arg in
      let tr, s3 = fresh_var s2 in
      let* s4 = unify s3 tf (TFun (ta, tr)) in
      return (tr, s4)
    | FunExpr (p, b) ->
      let rec bind ps st en acc =
        match ps with
        | [] -> return (List.rev acc, en, st)
        | p :: r ->
          let tp, s = fresh_var st in
          let* en', s' = bind_pattern en s p tp in
          bind r s' en' (tp :: acc)
      in
      let* ts, benv, stp = bind p state env [] in
      let* tb, stb = infer benv stp b in
      return (List.fold_right (fun a r -> TFun (a, r)) ts tb, stb)
    | Let (rf, p, d, b) ->
      (match rf with
       | NonRec ->
         let* td, s1 = infer env state d in
         let sch = generalize env s1 td in
         let* ne, s2 = bind_pattern_scheme env s1 p sch in
         infer ne s2 b
       | Rec ->
         (match p with
          | PVar n ->
            let t, s1 = fresh_var state in
            let er = StrMap.add n (Scheme ([], t, [])) env in
            let* td, s2 = infer er s1 d in
            let* s3 = unify s2 t td in
            let sch = generalize env s3 t in
            infer (StrMap.add n sch env) s3 b
          | _ -> fail (Other "rec")))
    | New (cl, args) ->
      (match StrMap.find_opt cl state.ct with
       | None -> fail (ClassNotFound cl)
       | Some info ->
         if List.length args <> List.length info.params
         then
           fail
             (ConstructorError
                (Format.asprintf "Class %s needs %d args" cl (List.length info.params)))
         else (
           let rec chk_args es ts s =
             match es, ts with
             | [], [] -> return (TClass cl, s)
             | e :: er, t :: tr ->
               let* te, s1 = infer env s e in
               let* s2 = unify s1 te t in
               chk_args er tr s2
             | _ -> fail (Other "impossible")
           in
           chk_args args info.params state))
    | MethodCall (obj, m, args) ->
      let* to_obj, s1 = infer env state obj in
      let rec loop s acc = function
        | [] -> return (List.rev acc, s)
        | x :: r ->
          let* t, s' = infer env s x in
          loop s' (t :: acc) r
      in
      let* t_args, s2 = loop s1 [] args in
      let tr, s3 = fresh_var s2 in
      let t_norm = apply_subst s3.subst to_obj in
      (match t_norm with
       | TClass c ->
         let* s = verify_constraints s3 c [ { name = m; args = t_args; ret = tr } ] in
         return (tr, s)
       | TVar id ->
         let old = Option.value (IntMap.find_opt id s3.constraints) ~default:[] in
         return
           ( tr
           , { s3 with
               constraints =
                 IntMap.add
                   id
                   ({ name = m; args = t_args; ret = tr } :: old)
                   s3.constraints
             } )
       | _ -> fail (Other "method call on non-obj"))

  and bind_pattern env state pat t =
    match pat with
    | PAny -> return (env, state)
    | PUnit -> return (env, state)
    | PVar n -> return (StrMap.add n (Scheme ([], t, [])) env, state)
    | PTuple ps ->
      let rec loop ps st acc_ts en =
        match ps with
        | [] -> return (List.rev acc_ts, en, st)
        | p :: r ->
          let tp, s = fresh_var st in
          let* ne_env, s' = bind_pattern en s p tp in
          loop r s' (tp :: acc_ts) ne_env
      in
      let* ts, ne, sf = loop ps state [] env in
      let* sr = unify sf t (TTuple ts) in
      return (ne, sr)

  and bind_pattern_scheme env state pat sch =
    match pat with
    | PVar n -> return (StrMap.add n sch env, state)
    | _ ->
      let t, s = instantiate sch state in
      bind_pattern env s pat t
  ;;

  let std_env =
    let add_prim name t m = StrMap.add name (Scheme ([], t, [])) m in
    StrMap.empty
    |> add_prim "print_int" (TFun (TInt, TUnit))
    |> add_prim "print_endl" (TFun (TUnit, TUnit))
    |> add_prim "print_bool" (TFun (TBool, TUnit))
  ;;

  let apply_subst_scheme subst (Scheme (vars, t, cs)) =
    let new_t = apply_subst subst t in
    Scheme (vars, new_t, cs)
  ;;

  let apply_subst_class_info subst info =
    { info with
      params = List.map (apply_subst subst) info.params
    ; fields = StrMap.map (apply_subst_scheme subst) info.fields
    ; methods_sch = StrMap.map (apply_subst_scheme subst) info.methods_sch
    }
  ;;

  let check_program_internal prog =
    let ct = StrMap.empty in
    let env = std_env in
    let state = initial_state ct in
    let* ct_f, st1 =
      List.fold_left
        (fun acc item ->
          let* c, s = acc in
          match item with
          | ClassDef cd ->
            let rec gen_params ps s a =
              match ps with
              | [] -> return (List.rev a, s)
              | _ :: r ->
                let v, s' = fresh_var s in
                gen_params r s' (v :: a)
            in
            let* pts, s_p = gen_params cd.class_params s [] in
            let map_gen l s =
              List.fold_left
                (fun (m, s) n ->
                  let v, s' = fresh_var s in
                  StrMap.add n (Scheme ([], v, [])) m, s')
                (StrMap.empty, s)
                l
            in
            let f, s' = map_gen (List.map fst cd.fields) s_p in
            let m, s'' = map_gen (List.map (fun m -> m.method_name) cd.methods) s' in
            return
              ( StrMap.add
                  cd.class_name
                  { parent = Option.map fst cd.parent_class
                  ; params = pts
                  ; methods_sch = m
                  ; fields = f
                  }
                  c
              , s'' )
          | _ -> return (c, s))
        (return (ct, state))
        prog
    in
    let st1 = { st1 with ct = ct_f } in
    let rec check items env st =
      match items with
      | [] ->
        let clean_env = StrMap.map (apply_subst_scheme st.subst) env in
        let clean_ct = StrMap.map (apply_subst_class_info st.subst) st.ct in
        return (clean_env, clean_ct, st)
      | Value (rf, pat, e) :: rest ->
        (match rf with
         | NonRec ->
           let* t, s1 = infer env st e in
           let sch = generalize env s1 t in
           let* ne, s2 = bind_pattern_scheme env s1 pat sch in
           check rest ne s2
         | Rec ->
           (match pat with
            | PVar n ->
              let t, s1 = fresh_var st in
              let er = StrMap.add n (Scheme ([], t, [])) env in
              let* td, s2 = infer er s1 e in
              let* s3 = unify s2 t td in
              let sch = generalize env s3 t in
              check rest (StrMap.add n sch env) s3
            | _ -> fail (Other "rec")))
      | ClassDef cd :: rest ->
        let info = StrMap.find cd.class_name st.ct in
        let rec bparams ps ts e s =
          match ps, ts with
          | [], [] -> return (e, s)
          | p :: r, t :: tr ->
            let* ne, s' = bind_pattern e s p t in
            bparams r tr ne s'
          | _ -> fail (Other "internal")
        in
        let* env_cls, st0 = bparams cd.class_params info.params env st in
        let* st_p =
          match cd.parent_class with
          | None -> return st0
          | Some (p_name, args) ->
            (match StrMap.find_opt p_name st.ct with
             | None -> fail (ClassNotFound p_name)
             | Some p_info ->
               if List.length args <> List.length p_info.params
               then
                 fail
                   (ConstructorError
                      (Format.asprintf
                         "Class %s needs %d args"
                         p_name
                         (List.length p_info.params)))
               else (
                 let rec chk_args es ts s =
                   match es, ts with
                   | [], [] -> return s
                   | e :: er, t :: tr ->
                     let* te, s1 = infer env_cls s e in
                     let* s2 = unify s1 te t in
                     chk_args er tr s2
                   | _ -> fail (Other "impossible")
                 in
                 chk_args args p_info.params st0))
        in
        let* st_f =
          List.fold_left
            (fun acc (fn, fe) ->
              let* s = acc in
              let f_exp, s1 = instantiate (StrMap.find fn info.fields) s in
              let* ft, s2 = infer env_cls s1 fe in
              unify s2 ft f_exp)
            (return st_p)
            cd.fields
        in
        let rec get_all_fields cname =
          match StrMap.find_opt cname st.ct with
          | None -> StrMap.empty
          | Some ci ->
            let my_fields = ci.fields in
            (match ci.parent with
             | None -> my_fields
             | Some p ->
               let parent_fields = get_all_fields p in
               StrMap.fold StrMap.add my_fields parent_fields)
        in
        let all_visible_fields = get_all_fields cd.class_name in
        let* st_m =
          List.fold_left
            (fun acc m ->
              let* s = acc in
              let mex, s1 = instantiate (StrMap.find m.method_name info.methods_sch) s in
              let base_env =
                match cd.self_name with
                | Some n -> StrMap.add n (Scheme ([], TClass cd.class_name, [])) env
                | None -> env
              in
              let menv = StrMap.fold StrMap.add all_visible_fields base_env in
              let rec bp ps s e a =
                match ps with
                | [] -> return (List.rev a, e, s)
                | p :: r ->
                  let tp, s' = fresh_var s in
                  let* ne, s'' = bind_pattern e s' p tp in
                  bp r s'' ne (tp :: a)
              in
              let* pts, be, s2 = bp m.method_params s1 menv [] in
              let* bt, s3 = infer be s2 m.method_body in
              let* s4 = unify s3 (List.fold_right (fun a r -> TFun (a, r)) pts bt) mex in
              match cd.parent_class with
              | Some (p, _) ->
                (match find_method_scheme st.ct p m.method_name with
                 | Some parent_sch ->
                   let parent_t, s5 = instantiate parent_sch s4 in
                   unify s5 mex parent_t
                 | None -> return s4)
              | None -> return s4)
            (return st_f)
            cd.methods
        in
        check rest env st_m
    in
    check prog env st1
  ;;

  let print_program_type prog =
    let pp_ct fmt ct =
      StrMap.iter
        (fun name info ->
          let ps =
            if Base.List.is_empty info.params
            then ""
            else
              Format.asprintf
                "(%a)"
                (Format.pp_print_list
                   ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                   pp_type)
                info.params
          in
          Format.fprintf fmt "class %s%s = object\n" name ps;
          StrMap.iter
            (fun n s -> Format.fprintf fmt "    val %s : %a\n" n pp_scheme s)
            info.fields;
          StrMap.iter
            (fun n s -> Format.fprintf fmt "    method %s : %a\n" n pp_scheme s)
            info.methods_sch;
          Format.fprintf fmt "end\n")
        ct
    in
    let pp_env fmt env =
      StrMap.iter
        (fun n s ->
          if not (StrMap.mem n std_env)
          then Format.fprintf fmt "val %s : %a\n" n pp_scheme s)
        env
    in
    match check_program_internal prog with
    | Error e -> Error (Format.asprintf "%a" pp_error e)
    | Ok (env, ct_f, _) -> Ok (Format.asprintf "%a%a" pp_ct ct_f pp_env env)
  ;;

  let infer_expression e =
    let* t, s = infer StrMap.empty (initial_state StrMap.empty) e in
    return (Format.asprintf "%a" pp_type (apply_subst s.subst t))
  ;;
end
