[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Stdio

type args =
  { mutable ast : bool
  ; mutable steps : int
  ; mutable typecheck : bool
  }

let () =
  let pr_args = { ast = true; steps = max_int; typecheck = false } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> pr_args.ast <- false), ""
    ; "--steps", Arg.Int (fun n -> pr_args.steps <- n), ""
    ; "--typecheck", Arg.Unit (fun () -> pr_args.typecheck <- true), ""
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
  let typecheck =
    match pr_args.typecheck with
    | true ->
      (match Inferencer.typecheck_program expr with
       | Ok _ -> None
       | Error er -> Some (Inferencer.show_type_error er))
    | false -> None
  in
  match typecheck with
  | None ->
    (match Interpret.run_interpret expr pr_args.steps with
     | Ok expr -> Printf.printf "%s\n" (Print.print_ast expr)
     | Error er -> Printf.printf "%s\n" (Print.print_error er))
  | Some er -> Printf.printf "%s\n" er
;;
