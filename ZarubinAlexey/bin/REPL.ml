[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

(* Читаем всю программу из stdin *)
let read_all_stdin () : string = In_channel.input_all In_channel.stdin

let () =
  let input = read_all_stdin () in
  match Lambda_lib.Interpret.run_string input with
  | Result.Ok (n, out) ->
    (* печатаем всё, что накопила встроенная функция print *)
    List.iter out ~f:Stdlib.print_endline;
    (* затем печатаем итоговое значение *)
    Stdlib.print_endline (Int.to_string n)
  | Result.Error #Lambda_lib.Parser.error ->
    (* как и раньше, стабильная строка для парсинга *)
    Stdlib.print_endline "Parsing error";
    Stdlib.exit 1
  | Result.Error (#Lambda_lib.Interpret.error as e) ->
    (* сохраняем твой текущий стиль сообщений об ошибках интерпретатора *)
    (match e with
     | `UnknownVariable x -> Format.eprintf "Interpreter error: unknown variable %s\n%!" x
     | `IfConditionNotInt ->
       Format.eprintf "Interpreter error: if condition is not int\n%!"
     | `BinopOnNonInt -> Format.eprintf "Interpreter error: binop on non-int\n%!"
     | `NotAFunction -> Format.eprintf "Interpreter error: not a function\n%!"
     | `DivisionByZero -> Format.eprintf "Interpreter error: division by zero\n%!"
     | `ResultNotInt -> Format.eprintf "Interpreter error: result is not int\n%!"
     | `FixOnNonFunction -> Format.eprintf "Interpreter error: fix on non-function\n%!"
     | `PrintArgumentNotInt ->
       Format.eprintf "Interpreter error: print argument not int\n%!");
    Stdlib.exit 1
;;
