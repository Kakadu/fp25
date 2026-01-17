open Base
open MiniML
module Valuetree = Valuetree.Make (Monads.State) (Interpreter.Error)

let eval_expression, eval_structure =
  let open Interpreter.Make (Monads.State) in
  eval_expression, eval_structure
;;

let infer_expression, infer_structure =
  let open Inferencer.Make (Monads.State) in
  infer_expression, infer_structure
;;

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
       (match eval_expression ast with
        | Error err -> Format.printf "interpreter error: %a\n" Interpreter.Error.pp err
        | Ok value -> Format.printf "evaluated: %a\n" Valuetree.pp_value value)
     | Infer ->
       (match infer_expression ast with
        | Error err -> Format.printf "inferencer error: %a\n" Inferencer.Error.pp err
        | Ok ty -> Format.printf "inferred: %a" Typedtree.pp_ty ty))
;;

let run_stru ~mode text =
  match Parser.parse_structure text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Parsetree.pp_structure ast
     | Infer ->
       (match infer_structure ast with
        | Ok typed_stru ->
          Format.printf "typed structure:@.%a" Typedtree.pp_structure typed_stru
        | Error err -> Format.printf "inferencer error: %a" Inferencer.Error.pp err)
     | _ -> failwith "not implemented REPL.exe")
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
    ; "-patt", arg_type Patt, "\t pattern"
    ; "-core-type", arg_type CoreType, "\t core type"
    ; "-parse", arg_mode Parse, "\t parser"
    ; "-eval", arg_mode Eval, "\t interpreter"
    ; "-infer", arg_mode Infer, "\t inferencer"
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
