open Ast
open Utils

type ground = GInt | GBool | GFloat [@@deriving show]

type typ = TGround of ground | TVar of string | TArrow of typ * typ
[@@deriving show]

type scheme = { vars : string list; ty : typ } [@@deriving show]
type subst = typ Table.t [@@deriving show]
type state = { sub : subst; fresh : typ } [@@deriving show]
type 'a infresult = Failed of string | Ok of state * 'a [@@deriving show]
type 'a inf = state -> 'a infresult

let ( >>= ) =
 fun m f ->
  fun st -> match m st with Failed s -> Failed s | Ok (st', x) -> f x st'

let ( let* ) = ( >>= )
let return x = fun st -> Ok (st, x)
let read = fun st -> Ok (st, st)
let write = fun st -> fun (_ : state) -> Ok (st, ())
let fail s = fun _ -> Failed s
let run = fun f st -> f st
let initial_state = { sub = Table.empty; fresh = TVar "'a" }

let get_next_tv = function
  | TVar s ->
      let rec helper = function
        | '\'' :: tl -> '\'' :: helper tl
        | ('a' .. 'y' as c) :: [] -> [ get_next_letter c ]
        | _ -> [ '\''; 'a' ]
      in
      TVar (charlst_to_str (helper (str_to_charlst s)))
  | _ -> TVar "'a"

let rec check_occur tv tree =
  match tree with
  | TVar x -> tv = x
  | TGround _ -> false
  | TArrow (l, r) -> check_occur tv l || check_occur tv r

let fresh =
  let* { sub = sb; fresh = fr } = read in
  let new_fr = get_next_tv fr in
  let* () = write { sub = sb; fresh = new_fr } in
  return fr

let sub_extend tv tree =
  let* { sub = sb; fresh = fr } = read in
  let sb' = Table.extend tv tree sb in
  write { sub = sb'; fresh = fr }

let sub_lookup x =
  let* { sub = sb; fresh = _ } = read in
  return (Table.lookup x sb)

let rec walk ty gamma =
  match ty with
  | TVar x -> (
      let res = Table.lookup x gamma in
      match res with None -> ty | Some y -> y)
  | TArrow (l, r) -> TArrow (walk l gamma, walk r gamma)
  | TGround _ -> ty

let rec sub_walk ty =
  match ty with
  | TVar x -> (
      let* look = sub_lookup x in
      match look with
      | None -> return ty
      | Some y ->
          let* res = sub_walk y in
          return res)
  | TArrow (l, r) ->
      let* l' = sub_walk l in
      let* r' = sub_walk r in
      return (TArrow (l', r'))
  | TGround _ -> return ty

let instantiate sch =
  let { vars; ty } = sch in
  let rec make_env lst env =
    match lst with
    | [] -> return env
    | h :: t ->
        let* tv = fresh in
        let* env' = make_env t env in
        return (Table.extend h tv env')
  in
  let* env = make_env vars Table.empty in
  return (walk ty env)

let rec unify l r =
  match (l, r) with
  | TVar x, TVar y when x = y -> return ()
  | (TVar x as tv), _ -> (
      if check_occur x r then fail "Occurrence check failed"
      else
        let* look = sub_walk tv in
        match look with
        | tv' when tv' = tv ->
            let* () = sub_extend x r in
            return ()
        | sigma_x -> unify sigma_x r)
  | _, (TVar x as tv) -> (
      if check_occur x l then fail "Occurrence check failed"
      else
        let* look = sub_walk tv in
        match look with
        | tv' when tv' = tv ->
            let* () = sub_extend x l in
            return ()
        | sigma_x -> unify sigma_x l)
  | TArrow (x1, y1), TArrow (x2, y2) ->
      let* () = unify x1 x2 in
      unify y1 y2
  | TGround x, TGround y ->
      if x = y then return () else fail "Ground type mismatch"
  | _ -> fail "TODO"

let generalize env ty =
  let rec get_set env ty =
    match ty with
    | TVar x as tv -> (
        match Table.contains_value { vars = []; ty = tv } env with
        | false -> StrSet.add x StrSet.empty
        | true -> StrSet.empty)
    | TGround _ -> StrSet.empty
    | TArrow (l, r) -> StrSet.union (get_set env l) (get_set env r)
  in
  { vars = StrSet.to_list (get_set env ty); ty }

let rec infer env exp =
  match exp with
  | EConst e -> (
      match e with
      | IConst _ -> return (TGround GInt)
      | FConst _ -> return (TGround GFloat)
      | BConst _ -> return (TGround GBool))
  | EVar e -> (
      match Table.lookup e env with
      | None -> fail "Unbound value"
      | Some x ->
          let* inst = instantiate x in
          return inst)
  | EBinOp (op, l, r) -> (
      let* l' = infer env l in
      let* r' = infer env r in
      match op with
      | Add | Sub | Div | Mul ->
          let* () = unify l' r' in
          let* () = unify l' (TGround GInt) in
          sub_walk l'
      | Lt | Leq | Gt | Geq | Eq | Neq ->
          let* () = unify l' r' in
          let* _ = sub_walk l' in
          return (TGround GBool)
      | AddF | SubF | DivF | MulF ->
          let* () = unify l' r' in
          let* () = unify l' (TGround GFloat) in
          sub_walk l'
      | And | Or ->
          let* () = unify l' r' in
          let* () = unify l' (TGround GBool) in
          sub_walk l')
  | EFun (EVar x, body) ->
      let* tv = fresh in
      let env' = Table.extend x { vars = []; ty = tv } env in
      let* body' = infer env' body in
      sub_walk (TArrow (tv, body'))
  | EApp (l, r) ->
      let* l' = infer env l in
      let* r' = infer env r in
      let* tv = fresh in
      let* () = unify l' (TArrow (r', tv)) in
      sub_walk tv
  | EIf (cond, if_body, else_body) ->
      let* cond' = infer env cond in
      let* () = unify cond' (TGround GBool) in
      let* if_body' = infer env if_body in
      let* else_body' = infer env else_body in
      let* () = unify if_body' else_body' in
      sub_walk if_body'
  | ELet (Nonrecursive, Bind (EVar x, e1), e2) ->
      let* e1' = infer env e1 in
      let sch = generalize env e1' in
      let env' = Table.extend x sch env in
      let* e2' = infer env' e2 in
      sub_walk e2'
  | ELet (Recursive, Bind (EVar x, e1), e2) ->
      let* tv = fresh in
      let env' = Table.extend x { vars = []; ty = tv } env in
      let* e1' = infer env' e1 in
      let* () = unify e1' tv in
      let sch = generalize env' tv in
      let* e2' = infer (Table.extend x sch env') e2 in
      sub_walk e2'
  | _ -> fail "not implemented yet"

let run_infer exp = run (infer Table.empty exp) initial_state
