open Ast

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env
  | VBuiltin of (value -> value eval)

and env = (ident * value) list

and error =
  | Unbound_variable of ident
  | Expected_int of value
  | Expected_bool of value
  | Expected_function of value
  | Division_by_zero
  | Invalid_recursion of ident
  | Invalid_fix of value
  | Invalid_comparison of value * value
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
  | VBool true -> "true"
  | VBool false -> "false"
  | VUnit -> "()"
  | VClosure _ | VRecClosure _ | VBuiltin _ -> "<fun>"
;;

let string_of_error = function
  | Unbound_variable name -> Printf.sprintf "unbound variable: %s" name
  | Expected_int v -> Printf.sprintf "expected int, got %s" (string_of_value v)
  | Expected_bool v ->
    Printf.sprintf "expected bool or int, got %s" (string_of_value v)
  | Expected_function v -> Printf.sprintf "expected function, got %s" (string_of_value v)
  | Division_by_zero -> "division by zero"
  | Invalid_recursion name -> Printf.sprintf "let rec expects a function for %s" name
  | Invalid_fix v -> Printf.sprintf "fix expects a function, got %s" (string_of_value v)
  | Invalid_comparison (l, r) ->
    Printf.sprintf
      "invalid comparison between %s and %s"
      (string_of_value l)
      (string_of_value r)
  | Step_limit_exceeded -> "step limit exceeded"
;;

let value_of_const = function
  | Int n -> VInt n
  | Bool b -> VBool b
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

let expect_bool (v : value) : bool eval =
  match v with
  | VBool b -> return b
  | VInt n -> return (bool_of_int n)
  | _ -> fail (Expected_bool v)
;;

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
    let* cond_bool = expect_bool cond_val in
    if cond_bool
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
  | BinopBool (op, left, right) -> eval_bool op env left right
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
    let* b = expect_bool v in
    return (VBool (not b))

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
  match op with
  | Eq | Neq -> (
    match l, r with
    | VInt l_int, VInt r_int ->
      let b = if op = Eq then l_int = r_int else l_int <> r_int in
      return (VBool b)
    | VBool l_bool, VBool r_bool ->
      let b = if op = Eq then l_bool = r_bool else l_bool <> r_bool in
      return (VBool b)
    | _ -> fail (Invalid_comparison (l, r)))
  | Lt | Le | Gt | Ge ->
    let* l_int = expect_int l in
    let* r_int = expect_int r in
    let b =
      match op with
      | Lt -> l_int < r_int
      | Le -> l_int <= r_int
      | Gt -> l_int > r_int
      | Ge -> l_int >= r_int
      | Eq | Neq -> false
    in
    return (VBool b)

and eval_bool op env left right =
  let* l = eval env left in
  let* l_bool = expect_bool l in
  match op with
  | And ->
    if l_bool
    then (
      let* r = eval env right in
      let* r_bool = expect_bool r in
      return (VBool r_bool))
    else return (VBool false)
  | Or ->
    if l_bool
    then return (VBool true)
    else (
      let* r = eval env right in
      let* r_bool = expect_bool r in
      return (VBool r_bool))
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
