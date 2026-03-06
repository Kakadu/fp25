(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string
[@@deriving show { with_path = false }]

type error = IError of interpret_error [@@deriving show { with_path = false }]

let ( let* ) = Result.bind
let return x = Ok x

type 'a res = ('a, interpret_error) result
type adr = Adr of int [@@deriving show { with_path = false }]

module IdMap = Map.Make (struct
    type t = ident

    let compare = compare
  end)

module LocMap = Map.Make (Int)

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VNull
  | VObject of adr
[@@deriving show { with_path = false }]

type func =
  { params : ident list
  ; body : stmt
  }

type location = int

type var_info =
  { loc : location
  ; initialized : bool
  }

type env = var_info IdMap.t list
type func_env = (ident * func) list

type store =
  { mem : value LocMap.t
  ; next_loc : int
  }

type object_id = int
type field_value = value

type object_state =
  { obj_id : object_id
  ; fields : (ident * field_value) list
  }

(*TODO name*)
type class_def =
  { fields : (ident * _type * expr option * bool) list
  ; methods : (ident * func) list
  }

type runtime =
  { env : env
  ; fenv : func_env
  ; store : store
  ; objects : object_state list
  ; curr_object : object_id option
  ; class_def : class_def option
  ; static_fields : (ident * value) list
  }

let pp_value fmt = function
  | VInt i -> Format.fprintf fmt "%d" i
  | VBool b -> Format.fprintf fmt "%b" b
  | VChar c -> Format.fprintf fmt "'%c'" c
  | VString s -> Format.fprintf fmt "\"%S\"" s
  | VNull -> Format.fprintf fmt "null"
  | VObject (Adr a) -> Format.fprintf fmt "object@%d" a
;;

type exec_result =
  | Normal
  | Return of value
  | Break
  | Continue

let empty_runtime =
  { env = [ IdMap.empty ]
  ; fenv = []
  ; store = { next_loc = 0; mem = LocMap.empty }
  ; objects = []
  ; curr_object = None
  ; class_def = None
  ; static_fields = []
  }
;;

let string_of_ident (Id s) = s

let rec lookup_env id = function
  | [] -> Error (NoVariable ("variable not found: " ^ string_of_ident id))
  | scope :: rest ->
    (match IdMap.find_opt id scope with
     | Some var_info -> Ok var_info.loc
     | None -> lookup_env id rest)
;;

let check_initialized id env =
  let rec find_var = function
    | [] -> Error (NoVariable (string_of_ident id))
    | scope :: rest ->
      (match IdMap.find_opt id scope with
       | Some var_info ->
         if var_info.initialized
         then Ok ()
         else Error (OtherError "Value is not initialized")
       | None -> find_var rest)
  in
  find_var env
;;

let mark_initialized id env =
  let rec mark_in_scope = function
    | [] -> []
    | scope :: rest ->
      (match IdMap.find_opt id scope with
       | Some var_info ->
         let new_var_info = { var_info with initialized = true } in
         IdMap.add id new_var_info scope :: rest
       | None -> scope :: mark_in_scope rest)
  in
  mark_in_scope env
;;

let rec lookup_func_opt (id : ident) = function
  | [] -> None
  | (id1, v) :: _ when id1 = id -> Some v
  | _ :: rest -> lookup_func_opt id rest
;;

let lookup_store l store =
  match LocMap.find_opt l store.mem with
  | Some v -> Ok v
  | None -> Error (AddressNotFound l)
;;

let update_store l v store = { store with mem = LocMap.add l v store.mem }

let alloc v store =
  let loc = store.next_loc in
  let store = { mem = LocMap.add loc v store.mem; next_loc = loc + 1 } in
  loc, store
;;

let lookup_env_r (id : ident) (rt : runtime) = lookup_env id rt.env
let lookup_store_r l rt = lookup_store l rt.store
let update_store_r l v rt = { rt with store = update_store l v rt.store }

let alloc_r v rt =
  let loc, store2 = alloc v rt.store in
  loc, { rt with store = store2 }
;;

let value_of_val_type = function
  | ValInt i -> VInt i
  | ValChar c -> VChar c
  | ValBool b -> VBool b
  | ValString s -> VString s
  | ValNull -> VNull
;;

let string_of_ident = function
  | Id s -> s
;;

let ident_of_vardecl = function
  | Var (_, id) -> id
;;

let expect_bool = function
  | VBool b -> Ok b
  | _ -> Error TypeMismatch
;;

let expect_int = function
  | VInt i -> Ok i
  | _ -> Error TypeMismatch
;;

let add_var (id : ident) (loc : location) (env : env) =
  match env with
  | scope :: rest ->
    let var_info = { loc; initialized = false } in
    Ok (IdMap.add id var_info scope :: rest)
  | [] -> Error (VarDeclared (string_of_ident id))
;;

let push_scope env = Ok (IdMap.empty :: env)

let pop_scope = function
  | _ :: rest -> Ok rest
  | [] -> Error (OtherError "cannot pop scope")
;;

let var_field_of_ast = function
  | VarField (mods, TypeVar typ, id, init) ->
    Some
      ( id
      , typ
      , init
      , List.exists
          (function
            | MStatic -> true
            | _ -> false)
          mods )
  | Method _ -> None
;;

let method_of_ast = function
  | Method (_, _, id, Params params, body) ->
    let params_list = List.map (fun (Var (_, id)) -> id) params in
    Some (id, { params = params_list; body })
  | VarField _ -> None
;;

let class_of_ast (Class (_, _, fields)) =
  let fields_list = List.filter_map var_field_of_ast fields in
  let methods_list = List.filter_map method_of_ast fields in
  { fields = fields_list; methods = methods_list }
;;

let find_field obj_id field_id rt =
  match List.find_opt (fun o -> o.obj_id = obj_id) rt.objects with
  | None -> Error (OtherError "object not found")
  | Some obj ->
    (match List.find_opt (fun (id, _) -> id = field_id) obj.fields with
     | Some (_, v) -> Ok v
     | None -> Error (OtherError "field not found"))
;;

let update_field obj_id field_id new_value rt =
  let rec update_obj_list = function
    | [] -> []
    | obj :: rest when obj.obj_id = obj_id ->
      let new_fields =
        List.map
          (fun (id, v) -> if id = field_id then id, new_value else id, v)
          obj.fields
      in
      { obj with fields = new_fields } :: rest
    | obj :: rest -> obj :: update_obj_list rest
  in
  { rt with objects = update_obj_list rt.objects }
;;

let find_static_field field_id rt =
  match List.find_opt (fun (id, _) -> id = field_id) rt.static_fields with
  | Some (_, v) -> Ok v
  | None -> Error (NoVariable (string_of_ident field_id))
;;

let update_static_field field_id new_value rt =
  let rec update_static_list = function
    | [] -> [ field_id, new_value ]
    | (id, _) :: rest when id = field_id -> (id, new_value) :: rest
    | (id, v) :: rest -> (id, v) :: update_static_list rest
  in
  { rt with static_fields = update_static_list rt.static_fields }
;;

let rec eval_expr (rt : runtime) = function
  | EValue v -> return (value_of_val_type v, rt)
  | EId id ->
    (match lookup_env id rt.env with
     | Ok loc ->
       (match check_initialized id rt.env with
        | Ok () ->
          let* v = lookup_store_r loc rt in
          return (v, rt)
        | Error e -> Error e)
     | Error _ ->
       (match find_static_field id rt with
        | Ok v -> return (v, rt)
        | Error _ ->
          (match rt.curr_object with
           | None -> Error (NoVariable (string_of_ident id))
           | Some obj_id ->
             (match find_field obj_id id rt with
              | Ok v -> return (v, rt)
              | Error e -> Error e))))
  | EBinOp (OpAssign, left, right) ->
    let* v, rt1 = eval_expr rt right in
    (match left with
     | EId id ->
       (match lookup_env id rt1.env with
        | Ok loc ->
          let new_env = mark_initialized id rt1.env in
          let rt2 = { rt1 with env = new_env } in
          let rt3 = update_store_r loc v rt2 in
          return (v, rt3)
        | Error _ ->
          (match find_static_field id rt1 with
           | Ok _ ->
             let rt2 = update_static_field id v rt1 in
             return (v, rt2)
           | Error _ ->
             (match rt1.curr_object with
              | None -> Error (OtherError ("cannot assign to " ^ string_of_ident id))
              | Some obj_id ->
                let rt2 = update_field obj_id id v rt1 in
                return (v, rt2))))
     | _ -> Error TypeMismatch)
  | EBinOp (OpAnd, e1, e2) ->
    let* v1, rt1 = eval_expr rt e1 in
    (match v1 with
     | VBool false -> return (VBool false, rt1)
     | VBool true ->
       let* v2, rt2 = eval_expr rt1 e2 in
       (match v2 with
        | VBool b -> return (VBool b, rt2)
        | _ -> Error TypeMismatch)
     | _ -> Error TypeMismatch)
  | EBinOp (OpOr, e1, e2) ->
    let* v1, rt1 = eval_expr rt e1 in
    (match v1 with
     | VBool true -> return (VBool true, rt1)
     | VBool false ->
       let* v2, rt2 = eval_expr rt1 e2 in
       (match v2 with
        | VBool b -> return (VBool b, rt2)
        | _ -> Error TypeMismatch)
     | _ -> Error TypeMismatch)
  | EBinOp (op, e1, e2) ->
    let* v1, rt1 = eval_expr rt e1 in
    let* v2, rt2 = eval_expr rt1 e2 in
    eval_binop op v1 v2 rt2
  | EUnOp (OpNot, e) ->
    let* v, rt1 = eval_expr rt e in
    (match v with
     | VBool b -> return (VBool (not b), rt1)
     | _ -> Error TypeMismatch)
  | EUnOp (OpNeg, e) ->
    let* v, rt1 = eval_expr rt e in
    (match v with
     | VInt i -> return (VInt (-i), rt1)
     | _ -> Error TypeMismatch)
  | EFuncCall (fn_expr, Args args) ->
    (match fn_expr with
     | EId id ->
       (match lookup_func_opt id rt.fenv with
        | None -> Error (OtherError ("function not found: " ^ string_of_ident id))
        | Some f ->
          let rec eval_args rt = function
            | [] -> return ([], rt)
            | e :: rest ->
              let* v, rt1 = eval_expr rt e in
              let* vs, rt2 = eval_args rt1 rest in
              return (v :: vs, rt2)
          in
          let* arg_vals, rt2 = eval_args rt args in
          let* v, rt3 = call_function rt2 f arg_vals in
          return (v, rt3))
     | _ -> Error (OtherError "invalid function call"))
  | EArrayAccess _ -> Error NotImplemented
  | EAwait _ -> Error NotImplemented

and eval_binop op v1 v2 rt : (value * runtime) res =
  match op, v1, v2 with
  | OpAdd, VInt a, VInt b -> return (VInt (a + b), rt)
  | OpSub, VInt a, VInt b -> return (VInt (a - b), rt)
  | OpMul, VInt a, VInt b -> return (VInt (a * b), rt)
  | OpDiv, VInt a, VInt b when b <> 0 -> return (VInt (a / b), rt)
  | OpDiv, VInt _, VInt 0 -> Error (ImpossibleResult "Div by zero")
  | OpMod, VInt a, VInt b when b <> 0 -> return (VInt (a mod b), rt)
  | OpMod, VInt _, VInt 0 -> Error (ImpossibleResult "Mod by zero")
  | OpEqual, v1, v2 -> return (VBool (v1 = v2), rt)
  | OpNonEqual, v1, v2 -> return (VBool (v1 <> v2), rt)
  | OpLess, VInt a, VInt b -> return (VBool (a < b), rt)
  | OpMore, VInt a, VInt b -> return (VBool (a > b), rt)
  | OpLessEqual, VInt a, VInt b -> return (VBool (a <= b), rt)
  | OpMoreEqual, VInt a, VInt b -> return (VBool (a >= b), rt)
  | OpAnd, VBool a, VBool b -> return (VBool (a && b), rt)
  | OpOr, VBool a, VBool b -> return (VBool (a || b), rt)
  | _ -> Error NotImplemented

and call_function (rt : runtime) f args =
  let caller_env = rt.env in
  let caller_obj = rt.curr_object in
  let rec bind_params env params args rt =
    match params, args with
    | [], [] -> return ({ rt with env }, rt)
    | p :: ps, v :: vs ->
      let loc, rt1 = alloc_r v rt in
      let var_info = { loc; initialized = true } in
      let* env2 =
        match env with
        | scope :: rest -> Ok (IdMap.add p var_info scope :: rest)
        | [] -> Error (OtherError "empty environment in bind_params")
      in
      bind_params env2 ps vs rt1
    | _ -> Error (OtherError "argument mismatch")
  in
  let* rt_func, _ = bind_params [ IdMap.empty ] f.params args rt in
  let rt_with_this = { rt_func with curr_object = caller_obj } in
  let* rt2, flow = exec_stmt rt_with_this f.body in
  let restored_rt = { rt2 with env = caller_env; curr_object = caller_obj } in
  match flow with
  | Return v -> return (v, restored_rt)
  | Normal -> return (VNull, restored_rt)
  | Break | Continue -> Error (OtherError "break/continue outside loop")

and exec_stmt (rt : runtime) = function
  | SExpr e ->
    let* _, rt1 = eval_expr rt e in
    return (rt1, Normal)
  | SDecl (decl, init) ->
    let id = ident_of_vardecl decl in
    let* value, rt1 =
      match init with
      | None -> return (VNull, rt)
      | Some e -> eval_expr rt e
    in
    let loc, rt2 = alloc_r value rt1 in
    let* env3 = add_var id loc rt2.env in
    let env4 =
      match init with
      | Some _ -> mark_initialized id env3
      | None -> env3
    in
    let rt3 = { rt2 with env = env4 } in
    return (rt3, Normal)
  | SIf (cond, then_s, else_s) ->
    let* v, rt1 = eval_expr rt cond in
    (match v with
     | VBool true -> exec_stmt rt1 then_s
     | VBool false ->
       (match else_s with
        | None -> return (rt1, Normal)
        | Some s -> exec_stmt rt1 s)
     | _ -> Error TypeMismatch)
  | SWhile (cond, body) ->
    let rec loop rt =
      let* v, rt1 = eval_expr rt cond in
      match v with
      | VBool true ->
        let* rt2, r = exec_stmt rt1 body in
        (match r with
         | Normal -> loop rt2
         | Continue -> loop rt2
         | Break -> return (rt2, Normal)
         | Return v -> return (rt2, Return v))
      | VBool false -> return (rt1, Normal)
      | _ -> Error TypeMismatch
    in
    loop rt
  | SBlock stmts ->
    let* env1 = push_scope rt.env in
    let rt1 = { rt with env = env1 } in
    let* rt2, flow = exec_block rt1 stmts in
    let* env3 = pop_scope rt2.env in
    let rt3 = { rt2 with env = env3 } in
    return (rt3, flow)
  | SReturn None -> return (rt, Return VNull)
  | SReturn (Some e) ->
    let* v, rt1 = eval_expr rt e in
    return (rt1, Return v)
  | SBreak -> return (rt, Break)
  | SContinue -> return (rt, Continue)
  | SFor (init, cond, step, body) ->
    let* env0 = push_scope rt.env in
    let rt0 = { rt with env = env0 } in
    let* rt1 =
      match init with
      | None -> return rt0
      | Some s ->
        let* rt1, r = exec_stmt rt0 s in
        (match r with
         | Normal -> return rt1
         | _ -> Error (OtherError "invalid control flow in for init"))
    in
    let rec loop rt =
      let* cond_val, rt1 =
        match cond with
        | None -> return (VBool true, rt)
        | Some e -> eval_expr rt e
      in
      match cond_val with
      | VBool false -> return (rt1, Normal)
      | VBool true ->
        let* rt2, r = exec_stmt rt1 body in
        (match r with
         | Return v -> return (rt2, Return v)
         | Break -> return (rt2, Normal)
         | Continue | Normal ->
           let* rt3 =
             match step with
             | None -> return rt2
             | Some e ->
               let* _, rt = eval_expr rt2 e in
               return rt
           in
           loop rt3)
      | _ -> Error TypeMismatch
    in
    let* rt2, flow = loop rt1 in
    let* env3 = pop_scope rt2.env in
    let rt3 = { rt2 with env = env3 } in
    return (rt3, flow)

and exec_block rt = function
  | [] -> return (rt, Normal)
  | s :: rest ->
    let* rt1, r = exec_stmt rt s in
    (match r with
     | Normal -> exec_block rt1 rest
     | _ -> return (rt1, r))
;;

let rec init_static_fields rt fields acc =
  match fields with
  | [] -> Ok (rt, List.rev acc)
  | (id, typ, init_opt) :: rest ->
    let default_value =
      match typ with
      | TypeBase TypeInt -> VInt 0
      | TypeBase TypeBool -> VBool false
      | TypeBase TypeChar -> VChar '\x00'
      | TypeBase TypeString -> VString ""
      | TypeVoid -> VNull
    in
    let rt_with_field =
      { rt with static_fields = (id, default_value) :: rt.static_fields }
    in
    let* value, rt1 =
      match init_opt with
      | Some init_expr -> eval_expr rt_with_field init_expr
      | None -> return (default_value, rt_with_field)
    in
    let rt2 = update_static_field id value rt1 in
    init_static_fields rt2 rest ((id, value) :: acc)
;;

let rec init_instance_fields rt fields acc =
  match fields with
  | [] -> Ok (rt, List.rev acc)
  | (id, typ, init_opt) :: rest ->
    let* value, rt1 =
      match init_opt with
      | Some init_expr -> eval_expr rt init_expr
      | None ->
        let default =
          match typ with
          | TypeBase TypeInt -> VInt 0
          | TypeBase TypeBool -> VBool false
          | TypeBase TypeChar -> VChar '\x00'
          | TypeBase TypeString -> VString ""
          | TypeVoid -> VNull
        in
        return (default, rt)
    in
    init_instance_fields rt1 rest ((id, value) :: acc)
;;

let init_program (Class (_, name, fields)) =
  let class_def = class_of_ast (Class ([], name, fields)) in
  let rt = { empty_runtime with class_def = Some class_def } in
  let rt_with_methods =
    List.fold_left
      (fun rt (id, func) -> { rt with fenv = (id, func) :: rt.fenv })
      rt
      class_def.methods
  in
  let static_fields =
    List.filter (fun (_, _, _, is_static) -> is_static) class_def.fields
  in
  let instance_fields =
    List.filter (fun (_, _, _, is_static) -> not is_static) class_def.fields
  in
  let strip_static (id, typ, init, _) = id, typ, init in
  let static_field_infos = List.map strip_static static_fields in
  let instance_field_infos = List.map strip_static instance_fields in
  let* rt1, static_vals = init_static_fields rt_with_methods static_field_infos [] in
  let rt2 = { rt1 with static_fields = static_vals } in
  let* rt3, instance_vals = init_instance_fields rt2 instance_field_infos [] in
  let obj_id = 0 in
  let program_object = { obj_id; fields = instance_vals } in
  let rt4 = { rt3 with objects = [ program_object ]; curr_object = Some obj_id } in
  Ok (None, rt4)
;;

let interpret_program = function
  | Program cls ->
    (match init_program cls with
     | Ok (_, rt) ->
       (match rt.class_def with
        | Some class_def ->
          (match List.find_opt (fun (id, _) -> id = Id "Main") class_def.methods with
           | Some (_, main_func) ->
             let* v, _ = call_function rt main_func [] in
             Ok (Some v)
           | None -> Error (OtherError "Main method not found"))
        | None -> Error (OtherError "No class definition"))
     | Error e -> Error e)
;;

let interpret str =
  match apply_parser Parser.parse_prog str with
  | Ok prog -> interpret_program prog
  | Error e -> Error (OtherError e)
;;

(* TODO: combine repeated code into functions?
   unwrap_return
*)

(* TODO: error messages? *)
(*
   TODO: lambdas + closures
   arrays (1D) + new
   FIX BUGS (interpreter)
   Quicktests for parser (if time permits)
   pre/post increment/decrement
   LINQ (simple array queries)
   async/await (at least without lambdas)
*)
