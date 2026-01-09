open Miniml_lib

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let module I = Interpret.Interpret (Base.Result) in
  match Parser.parse input with
  | Result.Error e -> Format.printf "Parse error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    (match I.run 10000 ast with
     | Result.Ok n -> Format.printf "Result: %d\n%!" n
     | Result.Error e -> Format.printf "Error: %a\n%!" Interpret.pp_error e)
;;
