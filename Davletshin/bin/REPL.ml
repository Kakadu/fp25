[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable stop_after : stop_after
  }

let run_single dump_parsetree stop_after eval =
  let text = In_channel.(input_all stdin) |> String.trim in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Format.printf "Error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    if dump_parsetree then Format.printf "Parsed result: @[%a@]\n%!" Printast.pp_named ast;
    (match stop_after with
     | SA_parsing -> ()
     | SA_never ->
       let rez = eval ast in
       Format.printf "Evaluated result: %a\n%!" Pprintast.pp_hum rez)
;;

let () =
  let opts = { dump_parsetree = false; stop_after = SA_never } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ; ( "-stop-after"
        , String
            (function
              | "parsing" -> opts.stop_after <- SA_parsing
              | _ -> failwith "Bad argument of -stop-after")
        , "" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Positioned arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for Utyped Lambda Calculus"
  in
  run_single opts.dump_parsetree opts.stop_after (fun ast -> Lambda.cbv_strat ast)
;;
