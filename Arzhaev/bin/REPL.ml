[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Interpeterlib.Parser
open Interpeterlib.Interpreter
open Interpeterlib.TypeInference
open Interpeterlib.Utils
open Interpeterlib.Ast

type error =
  | Parse of parse_error
  | Type of infer_error
  | Runtime of runtime_error

type opts =
  { mutable dump_parsetree : bool
  ; mutable initial_steps : int
  }

let pp_error fmt = function
  | Parse e -> Format.fprintf fmt "Parse error: %a" pp_parse_error e
  | Type e -> Format.fprintf fmt "Type error: %a" pp_infer_error e
  | Runtime e -> Format.fprintf fmt "Runtime error: %a" pp_runtime_error e
;;

let run_line opts (env_val, env_ty) line =
  match parser line with
  | Failed err -> Error (Parse err)
  | Parsed (tl, _) ->
    (match run_infer tl env_ty with
     | Failed err -> Error (Type err)
     | Ok (_, (env_ty', ty_res)) ->
       (match run_eval tl env_val opts.initial_steps with
        | Failed err -> Error (Runtime err)
        | Ok (_, (env_val', v_res)) -> Ok (env_val', env_ty', tl, ty_res, v_res)))
;;

let repl opts =
  let rec loop env_val env_ty =
    match read_line () with
    | exception End_of_file -> ()
    | line ->
      let line = String.trim line in
      if line = ""
      then loop env_val env_ty
      else (
        let env_val', env_ty' =
          match run_line opts (env_val, env_ty) line with
          | Error err ->
            Format.printf "%a\n%!" pp_error err;
            env_val, env_ty
          | Ok (env_val', env_ty', tl, ty_res, v_res) ->
            if opts.dump_parsetree then Format.printf "AST: %a\n%!" pp_toplevel tl;
            Format.printf "%a\n%!" pp_toplevel_result ty_res;
            Format.printf "%a\n%!" pp_toplevel_value v_res;
            env_val', env_ty'
        in
        loop env_val' env_ty')
  in
  loop Table.empty Table.empty
;;

let () =
  let opts = { dump_parsetree = false; initial_steps = 10000 } in
  let open Stdlib.Arg in
  parse
    [ "-dparsetree", Unit (fun () -> opts.dump_parsetree <- true), "dump AST"
    ; "-steps", Int (fun n -> opts.initial_steps <- n), "initial step budget"
    ]
    (fun _ -> ())
    "miniML REPL";
  repl opts
;;
