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
  ]

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment
  | VBuiltin of string * (value -> (value, error) Base.Result.t)

and environment = (string * value) list

module Interpret (M : MONAD_FAIL) : sig
  val run : string Ast.t -> (value, error) M.t
end = struct
  open M

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
  let rec eval env = function
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
    (* Implementation of fix combinator for CBV (Call-By-Value).

       For a function f = λrec_fn. λarg. ...body...
       We want: fix f = λarg. ...body[rec_fn := fix f]...

       The implementation mirrors let rec:
       We create a cyclic closure where the inner function has access to itself
       through the environment. *)
    let fix_builtin =
      VBuiltin
        ( "fix"
        , fun f_val ->
            match f_val with
            | VClosure (rec_param, inner_fn, f_env) ->
              (* f is λrec_param. inner_fn
                 We need to evaluate inner_fn with rec_param bound to the result itself *)
              (match inner_fn with
               | Ast.Abs (param, body) ->
                 (* inner_fn = λparam.body
                    Result should be: λparam. body with rec_param bound to result *)
                 let rec result = VClosure (param, body, (rec_param, result) :: f_env) in
                 Base.Result.Ok result
               | _ ->
                 (* If inner_fn is not a lambda, we can't properly implement fix *)
                 Base.Result.Error (`TypeMismatch : [> error ]))
            | _ -> Base.Result.Error (`TypeMismatch : [> error ]) )
    in
    [ "fix", fix_builtin ]
  ;;

  let run expr = eval (initial_env ()) expr
end

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let open Base.Result in
  let rez =
    Parser.parse str
    >>= fun ast ->
    I.run ast |> Result.map_error ~f:(fun e -> (e :> [ Parser.error | error ]))
  in
  match rez with
  | Result.Ok (VInt n) -> Stdlib.Printf.printf "Success: %d\n" n
  | Result.Ok (VClosure _) -> Stdlib.Printf.printf "Success: <closure>\n"
  | Result.Ok (VBuiltin (name, _)) -> Stdlib.Printf.printf "Success: <builtin %s>\n" name
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
;;
