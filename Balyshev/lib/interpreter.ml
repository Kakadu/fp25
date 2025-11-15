open Parsetree
open Base

type value =
  | VConstant of constant
  | VTuple of value * value * value list
  | VFun of pattern * expression * environment
  | VConstruct of string * value option

and error =
  | Is_not_a_function of expression
  | Unbound_value of string
  | Type_mismatch of string
  | Division_by_zero
  | Not_implemented of string

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

open Format

let rec show_environment env =
  let bindings =
    Map.to_alist env |> List.map ~f:(fun (name, value) -> name ^ " = " ^ show_value value)
  in
  "{ " ^ String.concat ~sep:"; " bindings ^ " }"

and show_value = function
  | VConstant CUnit -> sprintf "()"
  | VConstant (CInt x) -> sprintf "%d" x
  | VConstant (CBool x) -> sprintf "%b" x
  | VTuple (a, b, xs) ->
    Stdlib.List.map (fun x -> show_value x) (a :: b :: xs)
    |> String.concat ~sep:", "
    |> sprintf "@[(%s)@]"
  | VConstruct ("::", Some (VTuple (head, tail, []))) ->
    let rec helper acc = function
      | VConstruct ("::", Some (VTuple (hd, tl, []))) -> helper (hd :: acc) tl
      | VConstruct ("[]", None) ->
        sprintf
          "@[[ %s ]@]"
          (String.concat ~sep:"; " (List.map ~f:show_value (head :: List.rev acc)))
      | _ as exp ->
        String.concat ~sep:" :: " (List.map ~f:show_value (head :: List.rev (exp :: acc)))
    in
    helper [] tail
  | VConstruct ("[]", None) -> sprintf "[]"
  | VConstruct (name, None) -> sprintf "%s" name
  | VConstruct (name, Some arg) -> sprintf "@[%s (%s)@]" name (show_value arg)
  | VFun (patt, expr, _) ->
    sprintf "(%s -> %s)" (Parsetree.show_pattern patt) (Parsetree.show_expression expr)
;;

let pp_value ppf value = fprintf ppf "%s" (show_value value)

let show_error : error -> string = function
  | Is_not_a_function expr ->
    sprintf "| %s | is not a function, it can not be applied" (show_expression expr)
  | Unbound_value name -> sprintf "unbound value: %s" name
  | Type_mismatch msg -> sprintf "type mismatch: %s" msg
  | Division_by_zero -> sprintf "division by zero"
  | Not_implemented s -> sprintf "not implemented in <%s>" s
;;

let pp_error ppf error = fprintf ppf "%s" (show_error error)

module Eval (M : Monads.STATE_MONAD) = struct
  open M

  let init_env : (string, 'ok, Base.String.comparator_witness) Base.Map.t =
    Base.Map.empty (module Base.String)
  ;;

  let from_env env key =
    match Map.find env key with
    | Some value -> return value
    | None -> fail (Unbound_value key)
  ;;

  let eval_binop op a b =
    match op, a, b with
    | Add, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CInt (a + b)))
    | Sub, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CInt (a - b)))
    | Mul, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CInt (a * b)))
    | Div, VConstant (CInt a), VConstant (CInt b) ->
      if b = 0 then fail Division_by_zero else return (VConstant (CInt (a / b)))
    | Lt, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a < b)))
    | Gt, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a > b)))
    | Le, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a <= b)))
    | Ge, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a >= b)))
    | Eq, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a == b)))
    | Eq, VConstant (CBool a), VConstant (CBool b) -> return (VConstant (CBool (a == b)))
    | Ne, VConstant (CInt a), VConstant (CInt b) -> return (VConstant (CBool (a <> b)))
    | Ne, VConstant (CBool a), VConstant (CBool b) -> return (VConstant (CBool (a != b)))
    | Eq, VConstant CUnit, VConstant CUnit -> return (VConstant (CBool true))
    | Ne, VConstant CUnit, VConstant CUnit -> return (VConstant (CBool false))
    | Cons, left, right -> return (VConstruct ("Cons", Some (VTuple (left, right, []))))
    | _ ->
      fail
        (Type_mismatch
           (sprintf "incompatible values: %s, %s" (show_value a) (show_value b)))
  ;;

  let eval_tuple env eval (a, b, xs) =
    let* a = eval env a in
    let* b = eval env b in
    let rec helper items acc =
      match items with
      | [] -> return (List.rev acc)
      | x :: xs ->
        let* exp = eval env x in
        helper xs (exp :: acc)
    in
    let* xs = helper xs [] in
    return (VTuple (a, b, xs))
  ;;

  let rec bind_to_env env eval patt value =
    match patt, value with
    | PAny, _ -> return env
    | PVar x, value -> Map.set env ~key:x ~data:value |> return
    | PConstruct (name, Some patt), value ->
      (match value with
       | VConstruct (name2, Some value) when String.equal name name2 ->
         bind_to_env env eval patt value
       | VConstruct (name2, _) ->
         fail (Type_mismatch (sprintf "different constructors: %s and %s" name name2))
       | _ -> fail (Type_mismatch (sprintf "constructor %s expected" name)))
    | PTuple (p1, p2, ps), VTuple (v1, v2, vs) ->
      let rec helper env = function
        | [], [] -> return env
        | p :: ps, v :: vs ->
          let* env' = bind_to_env env eval p v in
          helper env' (ps, vs)
        | _ -> fail (Type_mismatch "different tuple arities")
      in
      helper env (p1 :: p2 :: ps, v1 :: v2 :: vs)
    | PConstruct (name, None), _ ->
      fail (Type_mismatch (sprintf "can not bind constant constructor: %s" name))
    | _ -> fail (Type_mismatch "value does not match pattern")
  ;;

  let bind_name_to_env env name value = Base.Map.set env ~key:name ~data:value

  let rec eval_expression env = function
    | EVar name -> from_env env name
    | EConstant (_ as x) -> return (VConstant x)
    | EBinop (op, a, b) ->
      let* a = eval_expression env a in
      let* b = eval_expression env b in
      eval_binop op a b
    | ETuple (a, b, xs) -> eval_tuple env eval_expression (a, b, xs)
    | EConstruct (name, None) -> M.return (VConstruct (name, None))
    | EConstruct (name, Some arg) ->
      let* arg = eval_expression env arg in
      return (VConstruct (name, Some arg))
    | ELet (NonRecursive, (pe, pes), body) ->
      let helper env (patt, expr) =
        let* env = env in
        let* value = eval_expression env expr in
        bind_to_env env eval_expression patt value
      in
      let* env' = List.fold (pe :: pes) ~f:helper ~init:(return env) in
      eval_expression env' body
    | EApp (f, arg) ->
      let* arg' = eval_expression env arg in
      let* f' = eval_expression env f in
      (match f' with
       | VFun (patt, body, closure) ->
         let* env = bind_to_env closure eval_expression patt arg' in
         eval_expression env body
       | _ -> fail (Is_not_a_function f))
    | EFun (patt, expr) -> return (VFun (patt, expr, env))
    | EIf (cond, expr_then, expr_else) ->
      eval_expression env cond
      >>= (function
       | VConstant (CBool true) -> eval_expression env expr_then
       | VConstant (CBool false) -> eval_expression env expr_else
       | _ -> fail (Type_mismatch "boolean expr expected"))
    | ELet (Recursive, _, _) -> fail (Not_implemented "let rec in eval_expr")
    | EMatch _ -> fail (Not_implemented "match with in eval_expr")
  ;;

  let eval_expr expr =
    match M.run (eval_expression init_env expr) (return 0) with
    | Ok (_state, value) -> Ok value
    | Error err -> Error err
  ;;
end
