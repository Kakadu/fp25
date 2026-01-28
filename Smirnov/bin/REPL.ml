(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Mardukml_lib

let _ =
  let _ = Printf.printf "MardukML REPL\n\n" in
  while true do
    try
      let _ = Printf.printf "# " in
      let s = read_line () in
      if String.trim s = ""
      then ()
      else (
        let res, tp = Mardukml.interp s in
        Printf.printf
          "%s : %s\n%!"
          (Lambda.lterm_to_string_typed res tp)
          (Typing.type_to_string tp))
    with
    | End_of_file ->
      let () = Printf.printf "\n" in
      exit 0
    | Failure s -> Printf.printf "%s\n%!" s
    | e -> Printf.printf "Unknown error: %s\n%!" (Printexc.to_string e)
  done
;;
