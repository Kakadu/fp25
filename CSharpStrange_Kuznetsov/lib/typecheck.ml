(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Monads.TYPECHECK
open Common

let value_to_type = function
  | ValInt _ -> TypeBase TypeInt
  | ValChar _ -> TypeBase TypeChar
  | ValBool _ -> TypeBase TypeBool
  | ValString _ -> TypeBase TypeString
  | ValNull -> TypeBase TypeInt (* TODO separately? *)
;;

let string_of_ident (Id s) = s

let vartype_to_type = function
  | TypeVar t -> t
;;

let name_to_obj_ctx = read_local_el
let eq f e1 e2 = if f e1 e2 then return e1 else fail (TCError TypeMismatch)
let eq_type t1 t2 = eq equal__type t1 t2

let field_of_ast = function
  | VarField (mods, typ, id, init) ->
    let is_static =
      List.exists
        (function
          | MStatic -> true
          | _ -> false)
        mods
    in
    Ok
      { field_modifiers = mods
      ; field_type = typ
      ; field_name = id
      ; field_init = init
      ; is_static
      }
  | Method _ -> Error (TCError TypeMismatch)
;;

(* Expected field, got method *)

let method_of_ast = function
  | Ast.Method (mods, ret_type, id, pms, body) ->
    let is_static =
      List.exists
        (function
          | MStatic -> true
          | _ -> false)
        mods
    in
    let is_main = equal_ident id (Id "Main") in
    Ok
      { method_modifiers = mods
      ; method_return = ret_type
      ; method_name = id
      ; method_params = pms
      ; method_body = body
      ; is_static
      ; is_main
      }
  | Ast.VarField _ -> Error (TCError TypeMismatch)
;;

(* Expected method, got field *)

let get_class_memb id memb =
  match memb with
  | VarField (_, _, f_id, _) when equal_ident f_id id ->
    (match field_of_ast memb with
     | Ok f_info -> Some (TCField f_info)
     | Error _ -> None)
  | Method (_, _, m_id, _, _) when equal_ident m_id id ->
    (match method_of_ast memb with
     | Ok m_info -> Some (TCMethod m_info)
     | Error _ -> None)
  | _ -> None
;;

let find_memb_from_obj obj_id id =
  let find_memb b id f = List.find_map (f id) b in
  let find_class_memb b id = find_memb b id get_class_memb in
  read_global_el obj_id
  >>= function
  | TCClass (Class (_, _, b)) -> find_class_memb b id |> return
;;

let find_memb_type = function
  | TCLocalVar v -> return (vartype_to_type v.var_type)
  | TCField f -> return (vartype_to_type f.field_type)
  | TCMethod m -> return m.method_return
;;

let typecheck_method_args (Params params) (Args args) expr_tc =
  let params_to_list_of_type p =
    List.map
      (function
        | Var (t, _) -> vartype_to_type t)
      p
  in
  let args_to_list_of_type a = map (fun x -> expr_tc x >>= find_memb_type) a in
  let compare_two_lists l1 l2 eq rez =
    match List.compare_lengths l1 l2 with
    | 0 ->
      if List.equal eq l1 l2
      then return rez
      else fail (TCError (OtherError "Method invocation check error"))
    | _ -> fail (TCError (OtherError "Method invocation check error"))
  in
  args_to_list_of_type args
  >>= fun args ->
  compare_two_lists (params_to_list_of_type params) args equal__type params
;;

let find_expr_type e expr_tc = expr_tc e >>= find_memb_type

let typecheck_bin_op b e1 e2 expr_tc =
  let compare_two_expr_type e1 e2 =
    find_expr_type e1 expr_tc
    >>= fun e1 -> find_expr_type e2 expr_tc >>= fun e2 -> eq_type e1 e2
  in
  let compare_three_expr_type e1 e2 t =
    compare_two_expr_type e1 e2 >>= fun e -> eq_type e t
  in
  let return_rez rez =
    let var_info = { var_type = TypeVar rez; initialized = true } in
    return (TCLocalVar var_info)
  in
  match b with
  | OpAdd | OpMul | OpSub | OpDiv | OpMod ->
    compare_three_expr_type e1 e2 (TypeBase TypeInt) *> return_rez (TypeBase TypeInt)
  | OpLess | OpLessEqual | OpMore | OpMoreEqual ->
    compare_three_expr_type e1 e2 (TypeBase TypeInt) *> return_rez (TypeBase TypeBool)
  | OpEqual | OpNonEqual -> compare_two_expr_type e1 e2 *> return_rez (TypeBase TypeBool)
  | OpAnd | OpOr ->
    compare_three_expr_type e1 e2 (TypeBase TypeBool) *> return_rez (TypeBase TypeBool)
  | OpAssign ->
    find_expr_type e1 expr_tc >>= fun e -> compare_two_expr_type e1 e2 *> return_rez e
;;

let typecheck_un_op u e expr_tc =
  let tc_un_op u e =
    find_expr_type e expr_tc
    >>= fun t ->
    match u with
    | OpNot -> eq_type t (TypeBase TypeBool)
    | OpNeg -> eq_type t (TypeBase TypeInt)
  in
  tc_un_op u e
  >>= fun t ->
  let var_info = { var_type = TypeVar t; initialized = true } in
  return (TCLocalVar var_info)
;;

let tc_method_args (Params params) (Args args) expr_tc =
  let params_to_list_of_type p =
    List.map
      (function
        | Var (t, _) -> vartype_to_type t)
      p
  in
  let args_to_list_of_type a = map (fun x -> expr_tc x >>= find_memb_type) a in
  let compare_two_lists l1 l2 eq rez =
    match List.compare_lengths l1 l2 with
    | 0 ->
      if List.equal eq l1 l2
      then return rez
      else fail (TCError (OtherError "Method invocation check error"))
    | _ -> fail (TCError (OtherError "Method invocation check error"))
  in
  args_to_list_of_type args
  >>= fun args ->
  compare_two_lists (params_to_list_of_type params) args equal__type params
;;

let tc_method_invoke e args expr_tc =
  expr_tc e
  >>= function
  | TCMethod m ->
    tc_method_args m.method_params args expr_tc
    >>= fun _ ->
    (match m.method_return with
     | TypeBase t ->
       let var_info = { var_type = TypeVar (TypeBase t); initialized = true } in
       return (TCLocalVar var_info)
     | TypeVoid ->
       fail (TCError (OtherError "Void methods cannot be used in expressions")))
  | TCField _ -> fail (TCError (OtherError "Cannot call a field as a method"))
  | TCLocalVar _ -> fail (TCError (OtherError "Cannot call a variable as a method"))
;;

let check_initialized n =
  (* TODO: refactor to locals ?? *)
  read_local_el n
  >>= function
  | TCLocalVar v when v.initialized -> return ()
  | TCLocalVar _ -> fail (TCError (OtherError "Variable may be uninitialized"))
  | TCField _ -> return ()
  | TCMethod _ -> return ()
;;

let typecheck_expr =
  let rec tc_expr_ = function
    | EId n ->
      name_to_obj_ctx n
      >>= (fun ctx -> check_initialized n *> return ctx)
      <|> (get_curr_class_name
           >>= fun class_name ->
           find_memb_from_obj class_name n
           >>= function
           | Some memb -> return memb
           | None ->
             fail (TCError (OtherError ("Variable not found: " ^ string_of_ident n))))
    | EValue v ->
      let var_info = { var_type = TypeVar (value_to_type v); initialized = true } in
      return (TCLocalVar var_info)
    | EFuncCall (e, args) -> tc_method_invoke e args tc_expr_
    | EBinOp (b, e1, e2) -> typecheck_bin_op b e1 e2 tc_expr_
    | EUnOp (u, e) -> typecheck_un_op u e tc_expr_
    | _ -> fail (TCError NotImplemented)
  in
  tc_expr_
;;

let typecheck_expr_with_type e = typecheck_expr e >>= find_memb_type
let eq_type_with_expr t e = typecheck_expr_with_type e >>= fun e_t -> eq_type e_t t

let save_decl n ctx =
  read_local_el_opt n
  >>= function
  | None -> write_local_el n ctx
  | Some _ -> fail (TCError (OtherError "This variable is already declared"))
;;

let apply_local f = read_local >>= fun old_l -> f *> write_local old_l

let rec typecheck_stmt =
  let is_expr_bool e =
    typecheck_expr_with_type e >>= fun t -> eq_type t (TypeBase TypeBool)
  in
  let typecheck_stmt_expr expr =
    match expr with
    | EFuncCall (e, args) ->
      typecheck_expr e
      >>= (function
       | TCMethod { method_return = TypeVoid; method_params = pms; _ } ->
         typecheck_method_args pms args typecheck_expr *> return ()
       | TCMethod _ -> fail (TCError TypeMismatch)
       | _ -> fail (TCError TypeMismatch))
    | EBinOp (OpAssign, _, _) -> typecheck_expr expr *> return ()
    | _ -> fail (TCError TypeMismatch)
  in
  let save_decl n t initialized =
    read_local_el_opt n
    >>= function
    | None ->
      let var_info = { var_type = TypeVar t; initialized } in
      write_local_el n (TCLocalVar var_info)
    | Some _ -> fail (TCError (OtherError "This variable is already declared"))
  in
  let typecheck_decl t n = function
    | Some e -> eq_type_with_expr t e *> save_decl n t true *> return ()
    | None -> save_decl n t false *> return ()
  in
  let typecheck_return e_opt =
    read_meth_type
    >>= fun m_t ->
    match m_t, e_opt with
    | Some TypeVoid, None -> return ()
    | Some (TypeBase t), Some e ->
      (eq_type_with_expr (TypeBase t) e
       <|> fail (TCError (OtherError "Returned type does not match the function type")))
      *> return ()
    | _ -> fail (TCError TypeMismatch)
  in
  let opt_unpack f = function
    | None -> return ()
    | Some s -> f s *> return ()
  in
  let typecheck_for_state init cond iter =
    let typecheck_init = function
      | None -> return ()
      | Some (SDecl (Var (TypeVar t, n), e)) -> typecheck_decl t n e
      | _ -> fail (TCError TypeMismatch)
    in
    let typecheck_cond = opt_unpack is_expr_bool cond in
    let typecheck_iter = opt_unpack typecheck_stmt_expr iter in
    lift3 (fun _ _ _ -> ()) (typecheck_init init) typecheck_cond typecheck_iter
  in
  let typecheck_if_state cond b s_opt tc_st =
    let typecheck_cond = is_expr_bool cond in
    let typecheck_state = function
      | Some st -> tc_st st
      | None -> return ()
    in
    lift3 (fun _ _ _ -> ()) typecheck_cond (tc_st b) (typecheck_state s_opt)
  in
  function
  | SExpr expr -> typecheck_stmt_expr expr
  | SDecl (Var (TypeVar t, n), e) -> typecheck_decl t n e
  | SReturn e -> typecheck_return e
  | SWhile (e, s) -> apply_local (is_expr_bool e *> typecheck_stmt s)
  | SFor (init, cond, iter, b) ->
    apply_local (typecheck_for_state init cond iter *> typecheck_stmt b)
  | SIf (e, b, s_opt) -> apply_local (typecheck_if_state e b s_opt typecheck_stmt)
  | SBlock st_l -> apply_local (iter typecheck_stmt st_l)
  | SBreak | SContinue -> fail (TCError NotImplemented)
;;

let tc_member mem class_fields =
  let tc_class_field f_type = function
    | Some e -> eq_type_with_expr (vartype_to_type f_type) e *> return ()
    | None -> return ()
  in
  let save_params_to_l (Params params) =
    let f = function
      | Var (t, n) ->
        let var_info = { var_type = t; initialized = true } in
        write_local_el n (TCLocalVar var_info)
    in
    iter f params
  in
  let tc_meth typ params body class_fields =
    apply_local
      (let add_field_to_env = function
         | VarField (mods, field_typ, id, init) ->
           let field_info =
             { field_modifiers = mods
             ; field_type = field_typ
             ; field_name = id
             ; field_init = init
             ; is_static = false
             }
           in
           write_local_el id (TCField field_info)
         | Method _ -> return ()
       in
       iter add_field_to_env class_fields
       *> write_meth_type typ
       *> save_params_to_l params
       *> typecheck_stmt body)
  in
  let tc_class_method (mds, tp, id, pms, b) class_fields =
    match method_of_ast (Method (mds, tp, id, pms, b)) with
    | Ok m ->
      if m.is_main
      then (
        let is_valid_signature =
          mds = [ MStatic ]
          && pms = Params []
          &&
          match tp with
          | TypeBase TypeInt | TypeVoid -> true
          | _ -> false
        in
        if is_valid_signature
        then
          tc_meth tp (Params []) b class_fields *> read_main_class
          >>= function
          | None -> get_curr_class_name >>= fun n -> write_main_class (Some n)
          | Some _ -> fail (TCError (OtherError "Main method already exists"))
        else
          fail
            (TCError
               (OtherError "Main must be static, non-async, no params, return int/void")))
      else tc_meth tp pms b class_fields
    | Error e -> fail e
  in
  match mem with
  | VarField (_, tp, _, e_opt) -> tc_class_field tp e_opt
  | Method (mds, tp, id, pms, b) -> tc_class_method (mds, tp, id, pms, b) class_fields
;;

let save_global id ctx =
  read_global_el_opt id
  >>= function
  | None -> write_global_el id ctx
  | Some _ -> fail (TCError (OtherError "This variable is already declared"))
;;

let typecheck_obj cl =
  match cl with
  | Class (_, id, fields) ->
    let write_mems () =
      let f mem =
        match mem with
        | VarField (_, _, id, _) ->
          (match field_of_ast mem with
           | Ok field_info -> save_decl id (TCField field_info)
           | Error e -> fail e)
        | Method (_, _, id, _, _) ->
          (match method_of_ast mem with
           | Ok method_info -> save_decl id (TCMethod method_info)
           | Error e -> fail e)
      in
      iter f fields
    in
    let tc_member_with_fields mem = tc_member mem fields in
    let tc_mems = iter tc_member_with_fields fields in
    let save_class = save_global id (TCClass cl) in
    write_curr_class_name id
    *> apply_local (write_mems () *> save_class *> tc_mems)
    *> return ()
;;

let typecheck prog = run (typecheck_obj prog) (IdMap.empty, IdMap.empty, None, None, None)
let typecheck_main prog = typecheck prog |> fun ((_, _, _, _, main), res) -> main, res
