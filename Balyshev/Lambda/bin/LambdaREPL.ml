[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

type strategy =
  | CBN
  | CBV
  | NO
  | AO

type strategy_kind =
  | Small_step
  | Big_step

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable mode : strategy_kind * strategy
  ; mutable stop_after : stop_after
  ; mutable limit : Lambda.limit
  }

let big_step_evaluator = function
  | AO -> Lambda.ao_strat
  | NO -> Lambda.no_strat
  | CBN -> Lambda.cbn_strat
  | CBV -> Lambda.cbv_strat
;;

let small_step_evaluator st =
  let st = big_step_evaluator st in
  let rec helper expr =
    let expr, lim = Lambda.apply_strat st expr (Lambda.Limited 1) in
    Format.printf " -- %a\n%!" Pprintast.pp_hum expr;
    if lim = Lambda.Exhausted then helper expr else expr
  in
  let on_app _ f arg _ = helper (Ast.App (f, arg)), Lambda.Unlimited in
  let on_abs _ f x _ = helper (Ast.Abs (f, x)), Lambda.Unlimited in
  let on_var _ x _ = helper (Ast.Var x), Lambda.Unlimited in
  { Lambda.on_var; on_abs; on_app }
;;

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
       let rez, lim = eval ast in
       (match lim with
        | Lambda.Unlimited -> Format.printf "Evaluated!\nResult: %a\n%!" Pprintast.pp rez
        | Lambda.Exhausted ->
          Format.printf "Partial evaluated.\nResult: %a\n%!" Pprintast.pp rez
        | Lambda.Limited lim ->
          Format.printf
            "Evaluated! Reductions left: %d.\nResult: %a\n%!"
            lim
            Pprintast.pp
            rez))
;;

let () =
  let opts =
    { dump_parsetree = false
    ; mode = Big_step, NO
    ; stop_after = SA_never
    ; limit = Unlimited
    }
  in
  let pick_strategy stra () =
    let kind, _ = opts.mode in
    opts.mode <- kind, stra
  in
  let pick_step step () =
    let _, stra = opts.mode in
    opts.mode <- step, stra
  in
  let pick_limit n =
    opts.limit
    <- (if n < 0 then failwith "Limit should be not negative integer" else Limited n)
  in
  let () =
    let open Stdlib.Arg in
    parse
      [ "-lim", Int pick_limit, "Reductions limit"
      ; "-cbv", Unit (pick_strategy CBV), "Call-by-value strategy"
      ; "-cbn", Unit (pick_strategy CBN), "Call-by-name strategy"
      ; "-no", Unit (pick_strategy NO), "Normal Order strategy"
      ; "-ao", Unit (pick_strategy AO), "Applicative Order strategy"
      ; ( "-small"
        , Unit (pick_step Small_step)
        , "Small-step strategy kind (default is big-step)" )
      ; ( "-big"
        , Unit (pick_step Big_step)
        , "Small-step strategy kind (default is big-step)" )
      ; ( "-dparsetree"
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
  run_single opts.dump_parsetree opts.stop_after (fun ast ->
    let stra =
      match opts.mode with
      | Big_step, stra -> big_step_evaluator stra
      | Small_step, stra -> small_step_evaluator stra
    in
    Lambda.apply_strat stra ast opts.limit)
;;
