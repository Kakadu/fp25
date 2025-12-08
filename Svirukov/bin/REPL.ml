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

type error = string

module UnResult : sig
  type 'a t = ('a, error) Result.t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = ('a, error) Result.t

  let return x = Ok x
  let fail e = Error e

  let ( let* ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;
end

open UnResult

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
  let result =
    let* expr =
      let input = In_channel.(input_all stdin) |> Base.String.rstrip in
      match Parser.parse input with
      | Ok expr -> return expr
      | Error (`Parsing_error msg) -> fail msg
    in
    let _ = if pr_args.ast then Format.printf "%s\n" (Print.print_ast expr) in
    let* expr =
      if pr_args.typecheck
      then (
        match Inferencer.typecheck_program expr with
        | Ok _ -> return expr
        | Error er -> fail (Print.show_type_error er))
      else return expr
    in
    match Interpret.run_interpret expr pr_args.steps with
    | Ok expr' -> return expr'
    | Error er -> fail (Print.print_error er)
  in
  match result with
  | Ok expr -> Format.printf "%s\n" (Print.print_ast expr)
  | Error msg -> Format.printf "%s\n" msg
;;
