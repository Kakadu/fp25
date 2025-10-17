open Base
open MiniML

type ty =
  | Expr
  | Patt
  | Stru

type mode =
  | Parse
  | Eval

let run_expr ~mode ~dump text =
  match Parser.parse_expression text with
  | Error err -> Format.printf "parsing error: %s\n" err
  | Ok ast ->
    if dump then Format.printf "parsed: %a\n" Ast.pp_expression ast;
    (match mode with
     | Parse -> Format.printf "parsed: %a\n" Ast.pp_expression ast
     | Eval ->
       (match Interpreter.eval_expr ast with
        | Error err -> Format.printf "interpreter error: %a\n" Interpreter.pp_error err
        | Ok value -> Format.printf "evaluated: %a\n" Interpreter.pp_value value))
;;

[@@@ocaml.warning "-69"]

type opts =
  { mutable ty : ty
  ; mutable mode : mode
  ; mutable dump_ast : bool
  }

let () =
  let opts = { ty = Expr; mode = Eval; dump_ast = false } in
  let arg_mode v = Stdlib.Arg.Unit (fun () -> opts.mode <- v) in
  let arg_type v = Stdlib.Arg.Unit (fun () -> opts.ty <- v) in
  let arg_dump_ast v = Stdlib.Arg.Unit (fun () -> opts.dump_ast <- v) in
  Stdlib.Arg.parse
    [ "-expr", arg_type Expr, "\tparse expression"
    ; "-patt", arg_type Patt, "\tparse pattern"
    ; "-stru", arg_type Stru, "\tparse structure"
    ; "-parse", arg_mode Parse, "\tparser"
    ; "-eval", arg_mode Eval, "\tinterpreter"
    ; "-dump-ast", arg_dump_ast true, "\tdump ast"
    ]
    (fun _ -> assert false)
    "REPL";
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  Format.printf "%S\n%!" s;
  (match opts.ty with
   | Expr -> run_expr ~mode:opts.mode ~dump:opts.dump_ast
   | _ -> failwith "")
    s
;;
