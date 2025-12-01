open Ast

(** runtime values **)

type value =
  | VInt of int
  | VUnit
  | VClosure of name * expression * env
  | VBuiltin of (value -> value eval)

and env = (name * value ref) list

and eval_error =
  | Unbound_variable of name
  | Not_a_function of value
  | Not_an_int of value
  | Division_by_zero
  | Step_limit_exceeded
  | Fix_argument_shape  (** fix argument shape **)

(** monad result with fuel counter **)

and 'a eval = int -> (('a * int), eval_error) result
(** remaining fuel counter **)

let return (x : 'a) : 'a eval =
  fun fuel -> Ok (x, fuel)

let error (e : eval_error) : 'a eval =
  fun _fuel -> Error e

let bind (m : 'a eval) (f : 'a -> 'b eval) : 'b eval =
  fun fuel ->
    match m fuel with
    | Error e -> Error e
    | Ok (x, fuel') -> f x fuel'

let ( let* ) = bind

(** single evaluation step **)
let step : unit eval =
  fun fuel ->
    if fuel <= 0 then Error Step_limit_exceeded
    else Ok ((), fuel - 1)

(** environment helpers **)

let lookup (env : env) (x : name) : value eval =
  fun fuel ->
    match List.assoc_opt x env with
    | None -> Error (Unbound_variable x)
    | Some cell -> Ok (!cell, fuel)

let extend (env : env) (x : name) (v : value) : env =
  (x, ref v) :: env

(** arithmetic **)

let int_binop (op : operation_id) (n1 : int) (n2 : int) : int eval =
  match op with
  | OpAdd -> return (n1 + n2)
  | OpSub -> return (n1 - n2)
  | OpMul -> return (n1 * n2)
  | OpDiv ->
      if n2 = 0 then error Division_by_zero
      else return (n1 / n2)
  | OpEq  -> return (if n1 = n2 then 1 else 0)
  | OpGt  -> return (if n1 >  n2 then 1 else 0)
  | OpLt  -> return (if n1 <  n2 then 1 else 0)
  | OpGte -> return (if n1 >= n2 then 1 else 0)
  | OpLte -> return (if n1 <= n2 then 1 else 0)

let eval_binop (op : operation_id) (v1 : value) (v2 : value) : value eval =
  match v1, v2 with
  | VInt n1, VInt n2 ->
      let* n = int_binop op n1 n2 in
      return (VInt n)
  | _ ->
      (** expect int arguments **)
      error (Not_an_int v1)

(** function application **)

let rec apply (f : value) (arg : value) : value eval =
  let* () = step in
  match f with
  | VClosure (param, body, closure_env) ->
      let env' = extend closure_env param arg in
      eval env' body
  | VBuiltin g ->
      g arg
  | _ ->
      error (Not_a_function f)

(** eval **)

and eval (env : env) (e : expression) : value eval =
  let* () = step in
  match e with
  | Const (Int n) -> return (VInt n)
  | Const Unit    -> return VUnit

  | Var x ->
      lookup env x

  | Fun (param, body) ->
      (** closure keeps environment **)
      return (VClosure (param, body, env))

  | App (e1, e2) ->
      (** call by value evaluate function then argument **)
      let* v_fun = eval env e1 in
      let* v_arg = eval env e2 in
      apply v_fun v_arg

  | BinOp (op, e1, e2) ->
      let* v1 = eval env e1 in
      let* v2 = eval env e2 in
      eval_binop op v1 v2

  | If (cond, thn, els_opt) ->
      let* v_cond = eval env cond in
      (match v_cond with
       | VInt 0 ->
           (** false branch **)
           (match els_opt with
            | None -> return VUnit
            | Some e_else -> eval env e_else)
       | VInt _ ->
           (** true branch **)
           eval env thn
       | _ ->
           error (Not_an_int v_cond))

  | Let (scope, kind, name, rhs, body_opt) ->
      (match kind with
       | NonRec ->
           (** non recursive binding **)
           let* v_rhs = eval env rhs in
           let env' = extend env name v_rhs in
           (match body_opt with
            | Some body ->
                (** local let **)
                eval env' body
            | None ->
                (** top level value **)
                return v_rhs)

       | Rec ->
           (** recursive binding **)
           let cell = ref VUnit in
           let env' = (name, cell) :: env in
           let* v_rhs = eval env' rhs in
           cell := v_rhs;
           (match body_opt with
            | Some body ->
                eval env' body
            | None ->
                return v_rhs))

(** builtin fix **)
let builtin_fix : value =
  VBuiltin
    (fun f ->
       match f with
       | VClosure (self_name, Fun (arg_name, body), env_f) ->
           let rec v_self =
             VClosure (arg_name, body, (self_name, ref v_self) :: env_f)
           in
           return v_self
       | _ ->
           error Fix_argument_shape)

(** builtin printing **)

let builtin_print_int : value =
  VBuiltin
    (fun v ->
       match v with
       | VInt n ->
           print_int n;
           flush stdout;
           return VUnit
       | _ ->
           error (Not_an_int v))

let builtin_print_newline : value =
  VBuiltin
    (fun _ ->
       print_newline ();
       flush stdout;
       return VUnit)

(** initial environment **)

let initial_env : env =
  [ ("fix",           ref builtin_fix)
  ; ("print_int",     ref builtin_print_int)
  ; ("print_newline", ref builtin_print_newline)
  ]

(** run interpreter **)

let run ?(max_steps = 100_000) (e : expression) : (value, eval_error) result =
  match eval initial_env e max_steps with
  | Ok (v, _fuel_left) -> Ok v
  | Error err -> Error err

let string_of_value = function
  | VInt n -> string_of_int n
  | VUnit -> "()"
  | VClosure _ -> "<fun>"
  | VBuiltin _ -> "<builtin>"

let string_of_error = function
  | Unbound_variable x -> Printf.sprintf "Unbound variable %s" x
  | Not_a_function v -> Printf.sprintf "Not a function: %s" (string_of_value v)
  | Not_an_int v -> Printf.sprintf "Not an int: %s" (string_of_value v)
  | Division_by_zero -> "Division by zero"
  | Step_limit_exceeded -> "Step limit exceeded"
  | Fix_argument_shape -> "fix expects fun self -> fun x -> ..."

let max_steps_from_env default_steps =
  match Sys.getenv_opt "MINIML_MAX_STEPS" with
  | None -> default_steps
  | Some s ->
      (match int_of_string_opt s with
       | Some n when n > 0 -> n
       | _ -> default_steps)

let parse_and_run str =
  match Parser.parse str with
  | Error e ->
      Format.printf "%a\n%!" Parser.pp_error e
  | Ok ast ->
      let max_steps = max_steps_from_env 100_000 in
      (match run ~max_steps ast with
       | Ok v ->
           Printf.printf "%s\n%!" (string_of_value v)
       | Error err ->
           Printf.printf "Error: %s\n%!" (string_of_error err))
