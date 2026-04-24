open Interpeterlib.Ast
open Interpeterlib.Parser
open Interpeterlib.Interpreter
open Interpeterlib.TypeInference
open Interpeterlib.Utils

type opts =
  { mutable dump_parsetree : bool
  ; mutable initial_steps : int
  }

let run_line opts (env_val, env_ty) line =
  match parser line with
  | Failed msg ->
    Format.printf "Parse error: %s\n%!" msg;
    env_val, env_ty
  | Parsed (tl, _) ->
    if opts.dump_parsetree then Format.printf "AST parsed\n%!";
    (* type inference *)
    (match run_infer tl env_ty with
     | Failed msg ->
       Format.printf "Type error: %s\n%!" msg;
       env_val, env_ty
     | Ok (_, (env_ty', ty_res)) ->
       Format.printf "%a\n%!" pp_toplevel_result ty_res;
       (* evaluation *)
       (match run_eval tl env_val opts.initial_steps with
        | Failed msg ->
          Format.printf "Runtime error: %s\n%!" msg;
          env_val, env_ty'
        | Ok (_, (env_val', v_res)) ->
          Format.printf "%a\n%!" pp_toplevel_value v_res;
          env_val', env_ty'))
;;

let repl opts =
  let rec loop env_val env_ty =
    Format.printf ">>> %!";
    match read_line () with
    | exception End_of_file -> ()
    | line ->
      let env_val', env_ty' = run_line opts (env_val, env_ty) line in
      loop env_val' env_ty'
  in
  loop Table.empty Table.empty
;;

let () =
  let opts = { dump_parsetree = false; initial_steps = 100 } in
  let open Stdlib.Arg in
  parse
    [ "-dparsetree", Unit (fun () -> opts.dump_parsetree <- true), "dump AST"
    ; "-steps", Int (fun n -> opts.initial_steps <- n), "initial step budget"
    ]
    (fun _ -> ())
    "Mini language REPL";
  repl opts
;;
