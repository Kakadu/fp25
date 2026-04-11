open Ast
open Utils

type state = int [@@deriving show]
type 'a evalres = Failed of string | Ok of state * 'a [@@deriving show]
type 'a interp = state -> 'a evalres

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of closure
  | VUnit

and closure = {
  param : string;
  body : expr;
  env : value Table.t;
  label : reclabel;
}

and reclabel = Nonrec | Rec of string [@@deriving show]

let ( >>= ) =
 fun m f ->
  fun st -> match m st with Failed s -> Failed s | Ok (st', x) -> f x st'

let ( let* ) = ( >>= )
let return x = fun st -> Ok (st, x)
let read = fun st -> Ok (st, st)
let write = fun st -> fun (_ : state) -> Ok (st, ())
let fail s = fun _ -> Failed s
let run = fun f st -> f st

let step ev =
  let* res = ev in
  let* steps = read in
  let* () = write (steps + 1) in
  return res

let lookup env x =
  match Table.lookup x env with
  | Some res -> return res
  | None -> fail (Printf.sprintf "Unbound value: %s" x)

let initial_state = 0
let make_closure env param body label = VClosure { param; body; env; label }

let rec eval env exp =
  match exp with
  | EConst (IConst c) -> step (return (VInt c))
  | EConst (FConst c) -> step (return (VFloat c))
  | EConst (BConst c) -> step (return (VBool c))
  | EVar x ->
      let* v = lookup env x in
      step (return v)
  | EBinOp (op, l, r) -> eval_binop env op l r
  | EIf (cond, e1, e2) -> eval_if env cond e1 e2
  | EFun (EVar x, e) -> step (return (make_closure env x e Nonrec))
  | ELet (Nonrecursive, Bind (EVar x, e1), e2) ->
      let* v1 = step (eval env e1) in
      let env' = Table.extend x v1 env in
      let* res = step (eval env' e2) in
      step (return res)
  | ELet (Recursive, Bind (EVar x, e1), e2) ->
      let* v1 = step (eval (Table.extend x VUnit env) e1) in
      let v1' =
        match v1 with
        | VClosure { param; body; env; _ } ->
            VClosure { param; body; env; label = Rec x }
        | v -> v
      in
      let env' = Table.extend x v1' env in
      let* res = step (eval env' e2) in
      step (return res)
  | EApp (e1, e2) ->
      let* v1 = step (eval env e1) in
      let* v2 = step (eval env e2) in
      apply_closure v1 v2
  | _ -> fail "Not implemented"

and eval_binop env op e1 e2 =
  let* v1 = step (eval env e1) in
  let* v2 = step (eval env e2) in
  let have_same_val_type l r =
    match (l, r) with
    | VBool _, VBool _ | VInt _, VInt _ | VFloat _, VFloat _ -> true
    | _ -> false
  in
  match (op, v1, v2) with
  | Add, VInt l, VInt r -> step (return (VInt (l + r)))
  | Sub, VInt l, VInt r -> step (return (VInt (l - r)))
  | Mul, VInt l, VInt r -> step (return (VInt (l * r)))
  | Div, VInt l, VInt r ->
      if r = 0 then fail "division by zero" else return (VInt (l / r))
  | AddF, VFloat l, VFloat r -> step (return (VFloat (l +. r)))
  | SubF, VFloat l, VFloat r -> step (return (VFloat (l -. r)))
  | MulF, VFloat l, VFloat r -> step (return (VFloat (l *. r)))
  | DivF, VFloat l, VFloat r ->
      if r = 0. then fail "division by zero" else return (VFloat (l /. r))
  | Eq, l, r when have_same_val_type l r -> step (return (VBool (l = r)))
  | Neq, l, r when have_same_val_type l r -> step (return (VBool (l <> r)))
  | Lt, l, r when have_same_val_type l r -> step (return (VBool (l < r)))
  | Leq, l, r when have_same_val_type l r -> step (return (VBool (l <= r)))
  | Gt, l, r when have_same_val_type l r -> step (return (VBool (l > r)))
  | Geq, l, r when have_same_val_type l r -> step (return (VBool (l >= r)))
  | And, VBool l, VBool r -> step (return (VBool (l && r)))
  | Or, VBool l, VBool r -> step (return (VBool (l || r)))
  | _ -> fail "operand type mismatch"

and eval_if env cond e1 e2 =
  let* cond' = step (eval env cond) in
  match cond' with
  | VBool true -> step (eval env e1)
  | VBool false -> step (eval env e2)
  | _ -> fail "cond is expected to have bool type"

and apply_closure vfun varg =
  match vfun with
  | VClosure { param; body; env; label = Nonrec } ->
      let defenv' = Table.extend param varg env in
      step (eval defenv' body)
  | VClosure { param; body; env; label = Rec x } ->
      let defenv' = Table.extend param varg (Table.extend x vfun env) in
      step (eval defenv' body)
  | _ ->
      fail
        (Printf.sprintf "This is not a function: %s. It cannot be applied."
           (show_value vfun))

let run_eval exp = run (eval Table.empty exp) initial_state
