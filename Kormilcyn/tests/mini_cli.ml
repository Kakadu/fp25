open Miniml_lib

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  Interpret.parse_and_run input
;;
