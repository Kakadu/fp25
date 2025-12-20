[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Base
open Utils

type error =
  [ `UnknownVariable of string
  | `DivisionByZero
  | `TypeMismatch
  | `StepLimitExceeded
  ]

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment
  | VBuiltin of string * (value -> (value, error) Base.Result.t)
  | VUnit

and environment = (string * value) list

module Interpret (M : MONAD_FAIL) : sig
  val run : max_steps:int -> string Ast.t -> (value, error) M.t
end = struct
  open M

  (** Step counter to prevent infinite loops *)
  let step_counter = ref 0

  let max_steps_limit = ref 10000

  (** Increment step counter and check limit *)
  let check_step () =
    step_counter := !step_counter + 1;
    if !step_counter > !max_steps_limit then fail `StepLimitExceeded else return ()
  ;;

  (** Lookup variable in environment *)
  let lookup env name =
    match List.Assoc.find ~equal:String.equal env name with
    | Some v -> return v
    | None -> fail (`UnknownVariable name)
  ;;

  (** Evaluate binary operation *)
  let eval_binop op v1 v2 =
    match op, v1, v2 with
    | Ast.Add, VInt a, VInt b -> return (VInt (a + b))
    | Ast.Sub, VInt a, VInt b -> return (VInt (a - b))
    | Ast.Mul, VInt a, VInt b -> return (VInt (a * b))
    | Ast.Div, VInt _, VInt 0 -> fail `DivisionByZero
    | Ast.Div, VInt a, VInt b -> return (VInt (a / b))
    | Ast.Mod, VInt _, VInt 0 -> fail `DivisionByZero
    | Ast.Mod, VInt a, VInt b -> return (VInt (a mod b))
    | Ast.Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
    | Ast.Neq, VInt a, VInt b -> return (VInt (if a <> b then 1 else 0))
    | Ast.Lt, VInt a, VInt b -> return (VInt (if a < b then 1 else 0))
    | Ast.Gt, VInt a, VInt b -> return (VInt (if a > b then 1 else 0))
    | Ast.Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
    | Ast.Geq, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
    | _ -> fail `TypeMismatch
  ;;

  (** Main evaluation function *)
  let rec eval env expr =
    check_step ()
    >>= fun () ->
    match expr with
    | Ast.Int n -> return (VInt n)
    | Ast.Var name -> lookup env name
    | Ast.Abs (param, body) -> return (VClosure (param, body, env))
    | Ast.BinOp (op, l, r) ->
      eval env l >>= fun vl -> eval env r >>= fun vr -> eval_binop op vl vr
    | Ast.If (cond, then_branch, else_branch_opt) ->
      eval env cond
      >>= (function
       | VInt 0 ->
         (match else_branch_opt with
          | Some else_branch -> eval env else_branch
          | None -> return (VInt 0))
       | VInt _ -> eval env then_branch
       | _ -> fail `TypeMismatch)
    | Ast.Let (is_rec, name, binding, body) ->
      if is_rec
      then (
        (* For recursive let, create environment with self-reference *)
        match binding with
        | Ast.Abs (param, func_body) ->
          (* Create closure with recursive environment *)
          let rec new_env = (name, VClosure (param, func_body, new_env)) :: env in
          eval new_env body
        | _ ->
          (* Non-function recursive bindings not supported yet *)
          eval env binding
          >>= fun value ->
          let new_env = (name, value) :: env in
          eval new_env body)
      else
        eval env binding
        >>= fun value ->
        let new_env = (name, value) :: env in
        eval new_env body
    | Ast.App (f, arg) ->
      eval env f
      >>= (function
       | VClosure (param, body, closure_env) ->
         eval env arg
         >>= fun varg ->
         let new_env = (param, varg) :: closure_env in
         eval new_env body
       | VBuiltin (_, builtin_fn) ->
         eval env arg
         >>= fun varg ->
         (match builtin_fn varg with
          | Base.Result.Ok v -> return v
          | Base.Result.Error e -> fail e)
       | _ -> fail `TypeMismatch)
  ;;

  (** Create initial environment with built-in functions *)
  let initial_env () =
    (* Print function: prints a value and returns unit *)
    let print_builtin =
      VBuiltin
        ( "print"
        , fun v ->
            (match v with
             | VInt n -> Stdlib.Printf.printf "%d\n%!" n
             | VClosure _ -> Stdlib.Printf.printf "<fun>\n%!"
             | VBuiltin (name, _) -> Stdlib.Printf.printf "<builtin:%s>\n%!" name
             | VUnit -> Stdlib.Printf.printf "()\n%!");
            Base.Result.Ok VUnit )
    in
    [ "print", print_builtin ]
  ;;

  let run ~max_steps expr =
    (* Reset and set step limit *)
    step_counter := 0;
    max_steps_limit := max_steps;
    eval (initial_env ()) expr
  ;;
end

(** Public function to evaluate an AST expression *)
let eval_expr ?(max_steps = 10000) ast =
  let module I = Interpret (Base.Result) in
  I.run ~max_steps ast
;;

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let open Base.Result in
  let rez =
    Parser.parse str
    >>= fun ast ->
    I.run ~max_steps:10000 ast
    |> Result.map_error ~f:(fun e -> (e :> [ Parser.error | error ]))
  in
  match rez with
  | Result.Ok (VInt n) -> Stdlib.Printf.printf "Success: %d\n" n
  | Result.Ok (VClosure _) -> Stdlib.Printf.printf "Success: <closure>\n"
  | Result.Ok (VBuiltin (name, _)) -> Stdlib.Printf.printf "Success: <builtin %s>\n" name
  | Result.Ok VUnit -> Stdlib.Printf.printf "Success: ()\n"
  | Result.Error (#Parser.error as e) ->
    (match e with
     | `Parsing_error msg -> Format.eprintf "Parsing error: %s\n%!" msg);
    Stdlib.exit 1
  | Result.Error (`UnknownVariable name) ->
    Format.eprintf "Interpreter error: Unknown variable %s\n%!" name;
    Stdlib.exit 1
  | Result.Error `DivisionByZero ->
    Format.eprintf "Interpreter error: Division by zero\n%!";
    Stdlib.exit 1
  | Result.Error `TypeMismatch ->
    Format.eprintf "Interpreter error: Type mismatch\n%!";
    Stdlib.exit 1
  | Result.Error `StepLimitExceeded ->
    Format.eprintf "Interpreter error: Step limit exceeded\n%!";
    Stdlib.exit 1
;;
