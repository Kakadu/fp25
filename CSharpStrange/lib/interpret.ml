(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

(* TODO: refactor Common *)
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

let ( let* ) = Result.bind (* TODO: into monad *)
let return x = Ok x

type 'a res = ('a, interpret_error) result (* TODO: name? *)
type adr = Adr of int (* TODO?? *) [@@deriving show { with_path = false }]

module IdMap = Map.Make (struct
  type t = ident

  let compare = compare
end)

module LocMap = Map.Make (Int) (* TODO: check Common *)

(* TODO: check if unite with Ast *)
type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VNull
  | VObject of adr
[@@deriving show { with_path = false }]

and func = { params : ident list; body : stmt }

type location = int
type env = location IdMap.t list (* scope stack *)
type func_env = (ident * func) list (* TODO: адреса ???*)
type store = { mem : value LocMap.t; next_loc : int }

(* Class *)
type object_id = int
type field_value = value
type object_state = { obj_id : object_id; fields : (ident * field_value) list }

type class_def = {
  name : ident;
  fields : (ident * _type * expr option) list;
  methods : (ident * func) list;
}

(* end class *)

(* TODO: state *)
type runtime = {
  env : env;
  fenv : func_env;
  store : store;
  objects : object_state list;
  curr_object : object_id option;
  class_def : class_def option;
}

(* Pp value *)
let rec pp_value fmt = function
  | VInt i -> Format.fprintf fmt "%d" i
  | VBool b -> Format.fprintf fmt "%b" b
  | VChar c -> Format.fprintf fmt "'%c'" c
  | VString s -> Format.fprintf fmt "\"%s\"" s
  | VNull -> Format.fprintf fmt "null"
  | VObject (Adr a) -> Format.fprintf fmt "object@%d" a

type exec_result = Normal | Return of value | Break | Continue

let empty_runtime =
  {
    env = [ IdMap.empty ];
    fenv = [];
    store = { next_loc = 0; mem = LocMap.empty };
    objects = [];
    curr_object = None;
    class_def = None;
  }
(* Functions *)

let string_of_ident (Id s) = s

let rec lookup_env id = function
  | [] -> Error (NoVariable ("variable not found: " ^ string_of_ident id))
  | scope :: rest -> (
      match IdMap.find_opt id scope with
      | Some l -> Ok l
      | None -> lookup_env id rest)

let rec lookup_func_opt (id : ident) = function
  | [] -> None
  | (id1, v) :: _ when id1 = id -> Some v
  | _ :: rest -> lookup_func_opt id rest

let lookup_store l store =
  match LocMap.find_opt l store.mem with
  | Some v -> Ok v
  | None -> Error (AddressNotFound l)
(*location not found *)

let update_store l v store = { store with mem = LocMap.add l v store.mem }

(* TODO: renaming *)
let alloc v store =
  let loc = store.next_loc in
  let store = { mem = LocMap.add loc v store.mem; next_loc = loc + 1 } in
  (loc, store)

(* TODO: make functions local *)
let lookup_env_r (id : ident) (rt : runtime) = lookup_env id rt.env
let lookup_store_r l rt = lookup_store l rt.store
let update_store_r l v rt = { rt with store = update_store l v rt.store }
(**)

let alloc_r v rt =
  let loc, store2 = alloc v rt.store in
  (loc, { rt with store = store2 })

let value_of_val_type = function
  | ValInt i -> VInt i
  | ValChar c -> VChar c
  | ValBool b -> VBool b
  | ValString s -> VString s
  | ValNull -> VNull

let string_of_ident = function Id s -> s
let ident_of_vardecl = function Var (_, id) -> id

(*expected bool*)
let expect_bool = function VBool b -> Ok b | _ -> Error TypeMismatch
let expect_int = function VInt i -> Ok i | _ -> Error TypeMismatch

let add_var (id : ident) (loc : location) (env : env) =
  match env with
  | scope :: rest -> Ok (IdMap.add id loc scope :: rest)
  | [] -> Error (VarDeclared (string_of_ident id))

let push_scope env = Ok (IdMap.empty :: env)

let pop_scope = function
  | _ :: rest -> Ok rest
  | [] -> Error (OtherError "cannot pop scope")

(* Class functions *)
let var_field_of_ast = function
  | VarField (mods, TypeVar typ, id, init) -> Some (id, typ, init)
  | Method _ -> None

let method_of_ast = function
  | Method (mods, ret_type, id, Params params, body) ->
      let params_list = List.map (fun (Var (_, id)) -> id) params in
      Some (id, { params = params_list; body })
  | VarField _ -> None

let class_of_ast (Class (mods, name, fields)) =
  let fields_list = List.filter_map var_field_of_ast fields in
  let methods_list = List.filter_map method_of_ast fields in
  { name; fields = fields_list; methods = methods_list }

let find_field obj_id field_id rt =
  match List.find_opt (fun o -> o.obj_id = obj_id) rt.objects with
  | None -> Error (OtherError "object not found")
  | Some obj -> (
      match List.find_opt (fun (id, _) -> id = field_id) obj.fields with
      | Some (_, v) -> Ok v
      | None -> Error (OtherError "field not found"))

let update_field obj_id field_id new_value rt =
  let rec update_obj_list = function
    | [] -> []
    | obj :: rest when obj.obj_id = obj_id ->
        let new_fields =
          List.map
            (fun (id, v) -> if id = field_id then (id, new_value) else (id, v))
            obj.fields
        in
        { obj with fields = new_fields } :: rest
    | obj :: rest -> obj :: update_obj_list rest
  in
  { rt with objects = update_obj_list rt.objects }

(* evaluation *)

(* eval_expr : env -> func_env -> store -> expr -> value * store *)
(* TODO: move binops to one new function *)
let rec eval_expr (rt : runtime) = function
  | EValue v -> return (value_of_val_type v, rt)
  | EId id -> (
      match lookup_env_r id rt with
      | Ok loc ->
          let* v = lookup_store_r loc rt in
          return (v, rt)
      | Error _ -> (
          match rt.curr_object with
          | None -> Error (NoVariable (string_of_ident id))
          | Some obj_id -> (
              match find_field obj_id id rt with
              | Ok v -> return (v, rt)
              | Error e -> Error e)))
  | EBinOp (OpAssign, left, right) -> (
      let* v, rt1 = eval_expr rt right in
      match left with
      | EId id -> (
          match lookup_env_r id rt1 with
          | Ok loc ->
              let rt2 = update_store_r loc v rt1 in
              return (v, rt2)
          | Error _ -> (
              match rt1.curr_object with
              | None ->
                  Error (OtherError ("cannot assign to " ^ string_of_ident id))
              | Some obj_id ->
                  let rt2 = update_field obj_id id v rt1 in
                  return (v, rt2)))
      | _ -> Error TypeMismatch)
  | EBinOp (OpAnd, e1, e2) -> (
      let* v1, rt1 = eval_expr rt e1 in
      match v1 with
      | VBool false -> return (VBool false, rt1)
      | VBool true -> (
          let* v2, rt2 = eval_expr rt1 e2 in
          match v2 with
          | VBool b -> return (VBool b, rt2)
          | _ -> Error TypeMismatch)
      | _ -> Error TypeMismatch)
  | EBinOp (OpOr, e1, e2) -> (
      let* v1, rt1 = eval_expr rt e1 in
      match v1 with
      | VBool true -> return (VBool true, rt1)
      | VBool false -> (
          let* v2, rt2 = eval_expr rt1 e2 in
          match v2 with
          | VBool b -> return (VBool b, rt2)
          | _ -> Error TypeMismatch)
      | _ -> Error TypeMismatch)
  | EBinOp (op, e1, e2) ->
      let* v1, rt1 = eval_expr rt e1 in
      let* v2, rt2 = eval_expr rt1 e2 in
      eval_binop op v1 v2 rt2
  | EUnOp (OpNot, e) -> (
      (* TODO separate funtion *)
      let* v, rt1 = eval_expr rt e in
      match v with
      | VBool b -> return (VBool (not b), rt1)
      | _ -> Error TypeMismatch)
  | EUnOp (OpNeg, e) -> (
      let* v, rt1 = eval_expr rt e in
      match v with VInt i -> return (VInt (-i), rt1) | _ -> Error TypeMismatch)
  | EFuncCall (fn_expr, Args args) -> (
      match fn_expr with
      | EId id -> (
          match lookup_func_opt id rt.fenv with
          | None ->
              Error (OtherError ("function not found: " ^ string_of_ident id))
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
      | _ -> Error (OtherError "invalid function call") (* TODO *))
  | EArrayAccess _ -> Error NotImplemented
  | EAwait _ -> Error NotImplemented

(* TODO: div 0 *)
(* TODO: other types binop & unop *)
and eval_binop op v1 v2 rt : (value * runtime) res =
  match (op, v1, v2) with
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
    match (params, args) with
    | [], [] -> return ({ rt with env }, rt)
    | p :: ps, v :: vs ->
        let loc, rt1 = alloc_r v rt in
        let* env2 =
          match env with
          | scope :: rest -> Ok (IdMap.add p loc scope :: rest)
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

(* exec_stmt : env -> func_env -> store -> stmt -> env * store * exec_result *)
and exec_stmt (rt : runtime) = function
  | SExpr e ->
      let* _, rt1 = eval_expr rt e in
      return (rt1, Normal)
  | SDecl (decl, init) ->
      let id = ident_of_vardecl decl in
      let* value, rt1 =
        match init with None -> return (VNull, rt) | Some e -> eval_expr rt e
      in
      let loc, rt2 = alloc_r value rt1 in
      let* env3 = add_var id loc rt2.env in
      let rt3 = { rt2 with env = env3 } in
      return (rt3, Normal)
  | SIf (cond, then_s, else_s) -> (
      let* v, rt1 = eval_expr rt cond in
      match v with
      | VBool true -> exec_stmt rt1 then_s
      | VBool false -> (
          match else_s with
          | None -> return (rt1, Normal)
          | Some s -> exec_stmt rt1 s)
      | _ -> Error TypeMismatch)
  | SWhile (cond, body) ->
      let rec loop rt =
        let* v, rt1 = eval_expr rt cond in
        match v with
        | VBool true -> (
            let* rt2, r = exec_stmt rt1 body in
            match r with
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
        | Some s -> (
            let* rt1, r = exec_stmt rt0 s in
            match r with
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
        | VBool true -> (
            let* rt2, r = exec_stmt rt1 body in
            match r with
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
  | s :: rest -> (
      let* rt1, r = exec_stmt rt s in
      match r with Normal -> exec_block rt1 rest | _ -> return (rt1, r))

let init_program (Class (_, name, fields)) =
  let class_def = class_of_ast (Class ([], name, fields)) in
  (* Creating runtime *)
  let rt = { empty_runtime with class_def = Some class_def } in
  (* Program initialization *)
  let rec init_fields rt fields acc =
    match fields with
    | [] -> Ok (rt, List.rev acc)
    | (id, typ, init_opt) :: rest ->
        (* При инициализации поля, другие поля уже должны быть в окружении *)
        let* value, rt1 =
          match init_opt with
          | Some init_expr ->
              (* Здесь init_expr может ссылаться на другие поля *)
              eval_expr rt init_expr
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
        init_fields rt1 rest ((id, value) :: acc)
  in
  let* rt1, fields_list = init_fields rt class_def.fields [] in
  (* Creating Program *)
  let obj_id = 0 in
  let program_object = { obj_id; fields = fields_list } in
  let rt2 =
    { rt1 with objects = [ program_object ]; curr_object = Some obj_id }
  in
  Ok (None, rt2)

let interpret_program prog =
  match prog with
  | Program cls -> (
      match init_program cls with
      | Ok (_, rt) -> (
          (* Find main *)
          match rt.class_def with
          | Some class_def -> (
              match
                List.find_opt (fun (id, _) -> id = Id "Main") class_def.methods
              with
              | Some (_, main_func) ->
                  let* v, _ = call_function rt main_func [] in
                  Ok (Some v)
              | None -> Error (OtherError "Main method not found"))
          | None -> Error (OtherError "No class definition"))
      | Error e -> Error e)

let interpret str =
  match apply_parser Parser.parse_prog str with
  | Ok prog -> interpret_program prog
  | Error e -> Error (OtherError e)

(* TODO: объединить повторы в функции
   unwrap_return
*)

(* TODO: errors texts *)

(*
   TODO: класс Program с методами и полями, а также модификаторами
         лямбды + что-нибудь с замыканиями
         пре и пост ин/декременты
         массивы (одномерные) + new
         LINQ (простенький в массивы)
         async/await (хотя бы без лямбд)

         ФИКС БАГОВ (Тч, парсер (Qt), интерпретатор)
*)
