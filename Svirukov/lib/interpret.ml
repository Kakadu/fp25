open Ast
open Base

type value =
  | VInt of int
  | VClosure of pattern * expr
  | VRecClosure of pattern * pattern * expr

and env = (string, value, Base.String.comparator_witness) Base.Map.t

type error =
  | UnboundVariable of string
  | TypeError of string
  | DivisionByZero
  | ParttialApplication
  | TooManyArgs
  | Unimplemented

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module ResultM : MONAD with type 'a t = ('a, error) result = struct
  type 'a t = ('a, error) result

  let return x = Ok x
  let fail msg = Error msg

  let ( >>= ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let ( let* ) = ( >>= )
end

let ( let* ) = ResultM.( >>= )

let ok_or_novar option err =
  match option with
  | Some v -> ResultM.return v
  | None -> ResultM.fail err
;;

module Env : sig
  val find : env -> string -> value option
  val init : env
  val add_val : env -> string -> value -> env
end = struct
  let init = Map.empty (module String)
  let find env key = Map.find env key
  let add_val env key value = Map.set env ~key ~data:value
end

let rec substitute expr varname value =
  match expr with
  | Constant _ -> expr
  | Var (PVar name) when Base.String.( = ) name varname -> value
  | Var _ -> expr
  | Binop (op, left, right) ->
    Binop (op, substitute left varname value, substitute right varname value)
  | Conditional (cond, then_expr, else_expr) ->
    Conditional
      ( substitute cond varname value
      , substitute then_expr varname value
      , match else_expr with
        | Some e -> Some (substitute e varname value)
        | None -> None )
  | Let (rec_flag, pattern, value_expr, body_expr) ->
    (match pattern with
     | PVar bound_name when Base.String.( = ) bound_name varname -> expr
     | _ ->
       Let
         ( rec_flag
         , pattern
         , substitute value_expr varname value
         , match body_expr with
           | Some e -> Some (substitute e varname value)
           | None -> None ))
  | Fun (PVar param, _) when Base.String.( = ) param varname -> expr
  | Fun (param, body) -> Fun (param, substitute body varname value)
  | App (func, arg) -> App (substitute func varname value, substitute arg varname value)
;;

let rec count_args expr n =
  match expr with
  | Fun (_, body) -> count_args body (n + 1)
  | expr -> expr, n
;;

(* удобно при спуске к самой Fun делать список из вычисленных аргументов *)
let rec eval exp env =
  match exp with
  | Constant CUnit -> ResultM.return (Constant CUnit)
  | Constant (CInt n) -> ResultM.return (Constant (CInt n))
  | Var (PVar name) ->
    let* binding = ok_or_novar (Env.find env name) (UnboundVariable name) in
    (match binding with
     | VInt n -> ResultM.return (Constant (CInt n))
     | VClosure (pat, body) -> ResultM.return (Fun (pat, body))
     | VRecClosure (_, _, _) -> ResultM.fail Unimplemented)
  | Binop (op, left, right) ->
    let* l =
      match eval left env with
      | Ok (Constant (CInt n)) -> ResultM.return n
      | Ok _ -> ResultM.fail (TypeError "Can do binop only with const int")
      | Error er -> ResultM.fail er
    in
    let* r =
      match eval right env with
      | Ok (Constant (CInt n)) -> ResultM.return n
      | Ok _ -> ResultM.fail (TypeError "Can do binop only with const int")
      | Error er -> ResultM.fail er
    in
    (match op with
     | Plus -> ResultM.return (Constant (CInt (l + r)))
     | Minus -> ResultM.return (Constant (CInt (l - r)))
     | Asteriks -> ResultM.return (Constant (CInt (l * r)))
     | Dash -> ResultM.return (Constant (CInt (l / r)))
     | Equals -> ResultM.return (Constant (CInt (if l = r then 1 else 0)))
     | MoreThan -> ResultM.return (Constant (CInt (if l > r then 1 else 0)))
     | LessThan -> ResultM.return (Constant (CInt (if l < r then 1 else 0)))
     | EqLess -> ResultM.return (Constant (CInt (if l <= r then 1 else 0)))
     | EqMore -> ResultM.return (Constant (CInt (if l >= r then 1 else 0))))
  (*TODO: don't know rec yet and something's wrong with functional types*)
  | Let (NonRec, PVar name, body, cont) ->
    let* body = eval body env in
    let letval =
      match body with
      | Constant (CInt n) -> VInt n
      | Fun (pat, inner) -> VClosure (pat, inner)
      | _ -> failwith "can put only vars and funcs in env"
    in
    let new_env = Env.add_val env name letval in
    (match cont with
     | Some exp -> eval exp new_env
     | None -> ResultM.return (Constant CUnit))
  | Conditional (cond, ifbr, elsebr) ->
    let* cond = eval cond env in
    (match cond with
     | Constant (CInt n) when n = 1 -> eval ifbr env
     | Constant (CInt n) when n = 0 ->
       (match elsebr with
        | Some e -> eval e env
        | None -> ResultM.return (Constant CUnit))
     | _ -> ResultM.fail (TypeError "not a number in cond evaluation"))
  | App (_, _) ->
    let rec application core env args =
      match args, core with
      | [], Fun (_, _) -> ResultM.fail ParttialApplication
      | [], expr -> eval expr env
      | arg :: tail, Fun (PVar name, body) ->
        let new_body = substitute body name arg in
        application new_body env tail
      | _ :: _, _ -> ResultM.fail TooManyArgs
    in
    let rec form_args expr list =
      match expr with
      | App (left, arg) ->
        let* arg = eval arg env in
        (match arg with
         | Constant (CInt _) -> form_args left (List.append [ arg ] list)
         | _ -> ResultM.fail TooManyArgs)
      | _ -> ResultM.return (list, expr)
    in
    let* args, body = form_args exp [] in
    application body env args
  | _ -> failwith "unimlemented"
;;

let run_interpret expr =
  match eval expr Env.init with
  | Ok exp -> Ok exp
  | Error er -> Error er
;;

let r = 7 * 8
