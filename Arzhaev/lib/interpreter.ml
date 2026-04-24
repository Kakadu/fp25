open Ast
open Utils

type state = int [@@deriving show]

type 'a evalres =
  | Failed of string
  | Ok of state * 'a
[@@deriving show]

type 'a interp = state -> 'a evalres

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of closure
  | VUnit

and closure =
  { param : string
  ; body : expr
  ; env : value Table.t
  ; label : reclabel
  }

and reclabel =
  | Nonrec
  | Rec of string
[@@deriving show]

type toplevel_value =
  | VLet of string * value
  | VExpr of value

let pp_value fmt = function
  | VInt x -> Format.fprintf fmt "%d" x
  | VFloat x -> Format.fprintf fmt "%f" x
  | VBool x -> Format.fprintf fmt "%b" x
  | VClosure _ -> Format.fprintf fmt "<fun>"
  | VUnit -> Format.fprintf fmt "()"
;;

let pp_toplevel_value fmt = function
  | VExpr v -> Format.fprintf fmt "- : %a" pp_value v
  | VLet (x, v) -> Format.fprintf fmt "val %s = %a" x pp_value v
;;

let ( >>= ) m f st =
  match m st with
  | Failed s -> Failed s
  | Ok (st', x) -> f x st'
;;

let ( let* ) = ( >>= )
let return x st = Ok (st, x)
let read st = Ok (st, st)
let write st (_ : state) = Ok (st, ())
let fail s _ = Failed s
let run f st = f st

let step =
  let* remaining_steps = read in
  if remaining_steps <= 0 then fail "step limit exceeded" else write (remaining_steps - 1)
;;

let lookup env x =
  match Table.lookup x env with
  | Some res -> return res
  | None -> fail (Printf.sprintf "Unbound value: %s" x)
;;

let initial_state = 100
let make_closure env param body label = VClosure { param; body; env; label }

let rec eval env exp =
  let* () = step in
  match exp with
  | EConst (IConst c) -> return (VInt c)
  | EConst (FConst c) -> return (VFloat c)
  | EConst (BConst c) -> return (VBool c)
  | EVar x ->
    let* v = lookup env x in
    return v
  | EBinOp (op, l, r) -> eval_binop env op l r
  | EIf (cond, e1, e2) -> eval_if env cond e1 e2
  | EFun (EVar x, e) -> return (make_closure env x e Nonrec)
  | ELet (Nonrecursive, Bind (EVar x, e1), e2) ->
    let* v1 = eval env e1 in
    let env' = Table.extend x v1 env in
    let* res = eval env' e2 in
    return res
  | ELet (Recursive, Bind (EVar x, e1), e2) ->
    let* v1 = eval (Table.extend x VUnit env) e1 in
    let v1' =
      match v1 with
      | VClosure { param; body; env; _ } -> VClosure { param; body; env; label = Rec x }
      | v -> v
    in
    let env' = Table.extend x v1' env in
    let* res = eval env' e2 in
    return res
  | EApp (e1, e2) ->
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    apply_closure v1 v2
  | _ -> fail "Not implemented"

and eval_binop env op e1 e2 =
  let* v1 = eval env e1 in
  let* v2 = eval env e2 in
  let have_same_val_type l r =
    match l, r with
    | VBool _, VBool _ | VInt _, VInt _ | VFloat _, VFloat _ -> true
    | _ -> false
  in
  match op, v1, v2 with
  | Add, VInt l, VInt r -> return (VInt (l + r))
  | Sub, VInt l, VInt r -> return (VInt (l - r))
  | Mul, VInt l, VInt r -> return (VInt (l * r))
  | Div, VInt l, VInt r ->
    if r = 0 then fail "division by zero" else return (VInt (l / r))
  | AddF, VFloat l, VFloat r -> return (VFloat (l +. r))
  | SubF, VFloat l, VFloat r -> return (VFloat (l -. r))
  | MulF, VFloat l, VFloat r -> return (VFloat (l *. r))
  | DivF, VFloat l, VFloat r ->
    if r = 0. then fail "division by zero" else return (VFloat (l /. r))
  | Eq, l, r when have_same_val_type l r -> return (VBool (l = r))
  | Neq, l, r when have_same_val_type l r -> return (VBool (l <> r))
  | Lt, l, r when have_same_val_type l r -> return (VBool (l < r))
  | Leq, l, r when have_same_val_type l r -> return (VBool (l <= r))
  | Gt, l, r when have_same_val_type l r -> return (VBool (l > r))
  | Geq, l, r when have_same_val_type l r -> return (VBool (l >= r))
  | And, VBool l, VBool r -> return (VBool (l && r))
  | Or, VBool l, VBool r -> return (VBool (l || r))
  | _ -> fail "operand type mismatch"

and eval_if env cond e1 e2 =
  let* () = step in
  let* cond' = eval env cond in
  match cond' with
  | VBool true -> eval env e1
  | VBool false -> eval env e2
  | _ -> fail "cond is expected to have bool type"

and apply_closure vfun varg =
  let* () = step in
  match vfun with
  | VClosure { param; body; env; label = Nonrec } ->
    let defenv' = Table.extend param varg env in
    eval defenv' body
  | VClosure { param; body; env; label = Rec x } ->
    let defenv' = Table.extend param varg (Table.extend x vfun env) in
    eval defenv' body
  | _ ->
    fail
      (Printf.sprintf
         "This is not a function: %s. It cannot be applied."
         (show_value vfun))
;;

let eval_toplevel env tl =
  match tl with
  | TopExpr e ->
    let* v = eval env e in
    return (env, VExpr v)
  | TopLet (Nonrecursive, Bind (EVar x, e)) ->
    let* v = eval env e in
    let env' = Table.extend x v env in
    return (env', VLet (x, v))
  | TopLet (Recursive, Bind (EVar x, e)) ->
    let env' = Table.extend x VUnit env in
    let* v = eval env' e in
    let v' =
      match v with
      | VClosure c -> VClosure { c with label = Rec x }
      | _ -> v
    in
    let env'' = Table.extend x v' env in
    return (env'', VLet (x, v'))
  | _ -> fail "unsupported toplevel"
;;

let run_eval tl env steps = run (eval_toplevel env tl) steps

let print_result = function
  | Ok (_, v) -> Format.printf "Value: %a\n%!" pp_value v
  | Failed msg -> Format.printf "Error: %s\n%!" msg
;;
