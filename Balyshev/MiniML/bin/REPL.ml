open Base
open MiniML
module Eval = Interpreter.Eval (Monads.State)
module Infer = Inferencer.Infer (Monads.State)

type ty =
  | Expr
  | Stru
  | Patt
  | CoreType

type mode =
  | Parse
  | Eval
  | Infer

let run_expr ~mode text =
  match Parser.parse_expression text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Parsetree.pp_expression ast
     | Eval ->
       (match Eval.eval_expr ast with
        | Error err -> Format.printf "interpreter error: %a\n" Interpreter.pp_error err
        | Ok value -> Format.printf "evaluated: %a\n" Interpreter.pp_value value)
     | Infer ->
       (match Infer.expression ast with
        | Error err -> Format.printf "inferencer error: %a\n" Inferencer.pp_error err
        | Ok ty -> Format.printf "inferred: %a" Typedtree.pp_ty ty))
;;

let run_stru ~mode text =
  match Parser.parse_structure text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Parsetree.pp_structure ast
     | _ -> failwith "not implemented")
;;

let run_patt ~mode text =
  match Parser.parse_pattern text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Parsetree.pp_pattern ast
     | _ -> failwith "Not supported")
;;

let run_core_type ~mode text =
  match Parser.parse_core_type text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Parsetree.pp_core_type ast
     | _ -> failwith "Not supported")
;;

type opts =
  { mutable ty : ty
  ; mutable mode : mode
  }

let () =
  let opts = { ty = Expr; mode = Eval } in
  let arg_mode v = Stdlib.Arg.Unit (fun () -> opts.mode <- v) in
  let arg_type v = Stdlib.Arg.Unit (fun () -> opts.ty <- v) in
  Stdlib.Arg.parse
    [ "-expr", arg_type Expr, "\t expression"
    ; "-stru", arg_type Stru, "\t structure"
    ; "-parse", arg_mode Parse, "\t parser"
    ; "-eval", arg_mode Eval, "\t interpreter"
    ; "-infer", arg_mode Infer, "\t inferencer"
    ; "-patt", arg_type Patt, "\t pattern"
    ; "-core-type", arg_type CoreType, "\t core type"
    ]
    (fun _ -> assert false)
    "REPL";
  (match opts.ty with
   | Expr -> run_expr ~mode:opts.mode
   | Patt -> run_patt ~mode:opts.mode
   | Stru -> run_stru ~mode:opts.mode
   | CoreType -> run_core_type ~mode:opts.mode)
    (Stdio.In_channel.(input_all stdin) |> String.rstrip)
;;
