open Ast

type value =
  | VInt of int
  | VUnit
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env
  | VBuiltin of (value -> value eval)

and env = (ident * value) list

and error =
  | Unbound_variable of ident
  | Expected_int of value
  | Expected_function of value
  | Division_by_zero
  | Invalid_recursion of ident
  | Invalid_fix of value
  | Step_limit_exceeded

and 'a eval = int -> (('a * int), error) result

let return x : 'a eval = fun fuel -> Ok (x, fuel)
let fail err : 'a eval = fun _ -> Error err

let ( let* ) (m : 'a eval) (f : 'a -> 'b eval) : 'b eval =
 fun fuel ->
  match m fuel with
  | Ok (x, fuel') -> f x fuel'
  | Error _ as err -> err
;;

let tick : unit eval = function
  | fuel when fuel <= 0 -> Error Step_limit_exceeded
  | fuel -> Ok ((), fuel - 1)
;;

let string_of_value = function
  | VInt n -> string_of_int n
  | VUnit -> "()"
  | VClosure _ | VRecClosure _ | VBuiltin _ -> "<fun>"
;;

let string_of_error = function
  | Unbound_variable name -> Printf.sprintf "unbound variable: %s" name
  | Expected_int v -> Printf.sprintf "expected int, got %s" (string_of_value v)
  | Expected_function v -> Printf.sprintf "expected function, got %s" (string_of_value v)
  | Division_by_zero -> "division by zero"
  | Invalid_recursion name -> Printf.sprintf "let rec expects a function for %s" name
  | Invalid_fix v -> Printf.sprintf "fix expects a function, got %s" (string_of_value v)
  | Step_limit_exceeded -> "step limit exceeded"
;;

let value_of_const = function
  | Int n -> VInt n
  | Bool true -> VInt 1
  | Bool false -> VInt 0
  | Unit () -> VUnit
;;

let lookup (env : env) (name : ident) : value eval =
  match List.assoc_opt name env with
  | Some v -> return v
  | None -> fail (Unbound_variable name)
;;

let expect_int (v : value) : int eval =
  match v with
  | VInt n -> return n
  | _ -> fail (Expected_int v)
;;

let bool_of_int n = n <> 0
let int_of_bool b = if b then 1 else 0

let rec eval (env : env) (expr : expr) : value eval =
  let* () = tick in
  match expr with
  | Const c -> return (value_of_const c)
  | Var name -> lookup env name
  | Lam (param, body) -> return (VClosure (param, body, env))
  | App (fn, arg) ->
    let* fn_val = eval env fn in
    let* arg_val = eval env arg in
    apply fn_val arg_val
  | Let (Nonrec, name, rhs, body) ->
    let* v = eval env rhs in
    eval ((name, v) :: env) body
  | Let (Rec, name, rhs, body) ->
    (match rhs with
     | Lam (param, lam_body) ->
       let self = VRecClosure (name, param, lam_body, env) in
       eval ((name, self) :: env) body
     | _ -> fail (Invalid_recursion name))
  | If (cond, then_e, else_e) ->
    let* cond_val = eval env cond in
    let* cond_int = expect_int cond_val in
    if bool_of_int cond_int
    then eval env then_e
    else (
      match else_e with
      | Some expr -> eval env expr
      | None -> return VUnit)
  | Unop (op, expr) ->
    let* v = eval env expr in
    eval_unop op v
  | BinopArithmetic (op, left, right) ->
    let* l = eval env left in
    let* r = eval env right in
    eval_arith op l r
  | BinopComp (op, left, right) ->
    let* l = eval env left in
    let* r = eval env right in
    eval_cmp op l r
  | Fix expr ->
    let* v = eval env expr in
    (match v with
     | VClosure (fname, body, closure_env) ->
       (match body with
        | Lam (param, lam_body) ->
          let self = VRecClosure (fname, param, lam_body, closure_env) in
          return self
        | _ -> fail (Invalid_fix v))
     | _ -> fail (Invalid_fix v))

and apply (fn_val : value) (arg_val : value) : value eval =
  match fn_val with
  | VClosure (param, body, closure_env) ->
    eval ((param, arg_val) :: closure_env) body
  | VRecClosure (fname, param, body, closure_env) ->
    eval ((param, arg_val) :: (fname, fn_val) :: closure_env) body
  | VBuiltin fn -> fn arg_val
  | _ -> fail (Expected_function fn_val)

and eval_unop op v =
  match op with
  | UMinus ->
    let* n = expect_int v in
    return (VInt (-n))
  | UPlus ->
    let* n = expect_int v in
    return (VInt n)
  | Not ->
    let* n = expect_int v in
    return (VInt (int_of_bool (not (bool_of_int n))))

and eval_arith op l r =
  let* l_int = expect_int l in
  let* r_int = expect_int r in
  match op with
  | Add -> return (VInt (l_int + r_int))
  | Sub -> return (VInt (l_int - r_int))
  | Mul -> return (VInt (l_int * r_int))
  | Div ->
    if r_int = 0
    then fail Division_by_zero
    else return (VInt (l_int / r_int))

and eval_cmp op l r =
  let* l_int = expect_int l in
  let* r_int = expect_int r in
  let b =
    match op with
    | Eq -> l_int = r_int
    | Neq -> l_int <> r_int
    | Lt -> l_int < r_int
    | Le -> l_int <= r_int
    | Gt -> l_int > r_int
    | Ge -> l_int >= r_int
  in
  return (VInt (int_of_bool b))
;;

let builtin_print_int =
  VBuiltin (fun v ->
    let* n = expect_int v in
    Printf.printf "%d\n%!" n;
    return VUnit)
;;

let initial_env : env = [ "print_int", builtin_print_int; "print", builtin_print_int ]

let eval_expr ?(fuel = 10_000) expr =
  match eval initial_env expr fuel with
  | Ok (v, _) -> Ok v
  | Error _ as err -> err
;;

let eval_toplevel (env : env) = function
  | TExpr expr ->
    let* v = eval env expr in
    return (env, Some v)
  | TLet (Nonrec, name, rhs) ->
    let* v = eval env rhs in
    return ((name, v) :: env, None)
  | TLet (Rec, name, rhs) ->
    (match rhs with
     | Lam (param, lam_body) ->
       let self = VRecClosure (name, param, lam_body, env) in
       return ((name, self) :: env, None)
     | _ -> fail (Invalid_recursion name))
;;

let eval_program ?(fuel = 10_000) (program : program) =
  let rec loop env outputs = function
    | [] -> return (List.rev outputs)
    | item :: rest ->
      let* (env', out) = eval_toplevel env item in
      let outputs' = match out with None -> outputs | Some v -> v :: outputs in
      loop env' outputs' rest
  in
  match loop initial_env [] program fuel with
  | Ok (values, _) -> Ok values
  | Error _ as err -> err
;;
