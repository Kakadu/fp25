[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

(** Читаем всю программу из stdin одной строкой.*)
let read_all_stdin () : string = In_channel.input_all In_channel.stdin

(** Дефолтный лимит шагов интерпретатора (fuel).
    Нужен, чтобы бесконечные программы (например, бесконечная рекурсия)
    не зависали и не вешали тесты/проверку. *)
let default_steps = 10_000

(** Пытаемся прочитать лимит шагов из argv.
    Поддерживаем два режима запуска:
     REPL.exe            -> используем default_steps
     REPL.exe 50000      -> используем steps = 50000
      Если аргумент не число или число <= 0, возвращаем default_steps. *)
let read_steps_from_argv () : int =
  match Stdlib.Sys.argv with
  | [| _ |] ->
    (* Аргументов нет: просто используем дефолт. *)
    default_steps
  | [| _; s |] ->
    (* Есть один аргумент: пробуем распарсить. *)
    (match Stdlib.int_of_string_opt s with
     | Some n when n > 0 -> n
     | _ -> default_steps)
  | _ ->
    (* Больше одного аргумента: считаем это некорректным вызовом,
       но для стабильности просто берём default_steps. *)
    default_steps
;;

let () =
  (* 1 Читаем лимит шагов (fuel). *)
  let steps = read_steps_from_argv () in
  (* 2 Читаем программу из stdin целиком. *)
  let input = read_all_stdin () in
  (* 3 Запускаем: парсинг + интерпретация. *)
  match Lambda_lib.Interpret.run_string_with_steps ~steps input with
  | Result.Ok (n, out) ->
    (* Успешно:
       сначала печатаем весь накопленный вывод print (в правильном порядке),
       потом печатаем итоговое значение программы. *)
    List.iter out ~f:Stdlib.print_endline;
    Stdlib.print_endline (Int.to_string n)
  | Result.Error (`Parsing_error _) ->
    (* Любая ошибка парсинга -> стабильное сообщение. *)
    Stdlib.print_endline "Parsing error";
    Stdlib.exit 1
  | Result.Error (#Lambda_lib.Interpret.error as e) ->
    (* Ошибки интерпретации печатаем в stderr,
       и завершаем процесс с ненулевым кодом. *)
    (match e with
     | `Parsing_error _ -> Stdlib.print_endline "Parsing error"
     | `UnknownVariable x -> Format.eprintf "Interpreter error: unknown variable %s\n%!" x
     | `IfConditionNotInt ->
       Format.eprintf "Interpreter error: if condition is not int\n%!"
     | `BinopOnNonInt -> Format.eprintf "Interpreter error: binop on non-int\n%!"
     | `NotAFunction -> Format.eprintf "Interpreter error: not a function\n%!"
     | `DivisionByZero -> Format.eprintf "Interpreter error: division by zero\n%!"
     | `ResultNotInt -> Format.eprintf "Interpreter error: result is not int\n%!"
     | `FixOnNonFunction -> Format.eprintf "Interpreter error: fix on non-function\n%!"
     | `PrintArgumentNotInt ->
       Format.eprintf "Interpreter error: print argument not int\n%!"
     | `StepLimitExceeded ->
       (* Ошибка fuel: это не логическая ошибка программы,
          а защита от зависаний. *)
       Format.eprintf "Interpreter error: step limit exceeded\n%!");
    Stdlib.exit 1
;;
