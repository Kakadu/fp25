[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Stdio

type args =
  { mutable ast : bool
  ; mutable steps : int
  }

let () =
  let pr_args = { ast = true; steps = max_int } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> pr_args.ast <- false), ""
    ; "--steps", Arg.Int (fun n -> pr_args.steps <- n), ""
    ]
  in
  let usage_msg = "miniMl starts...\n" in
  Arg.parse arg_list (fun _ -> ()) usage_msg;
  let expr =
    match Parser.parse (In_channel.(input_all stdin) |> Base.String.rstrip) with
    | Ok expr ->
      let _ = if pr_args.ast then Printf.printf "%s\n" (Print.print_ast expr) in
      expr
    | Error (`Parsing_error msg) -> failwith msg
  in
  match Interpret.run_interpret expr pr_args.steps with
  | Ok expr -> Printf.printf "%s\n" (Print.print_ast expr)
  | Error er -> Printf.printf "%s\n" (Print.print_error er)
;;
