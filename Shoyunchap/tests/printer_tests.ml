open Lambda_lib
open Parser
open Printer
open Quickcheck

let () =
  (* QuickCheck-подобный тест: печать + повторный парсинг стабилизируются *)
  check ~name:"printer_roundtrip"
    Quickcheck.Gen.(expr 3)
    (fun e ->
       let first = string_of_expr e in
       match parse first with
       | Error _ -> false
       | Ok e' ->
           let second = string_of_expr e' in
           match parse second with
           | Ok e'' ->
               let third = string_of_expr e'' in
               String.equal second third
           | Error _ -> false)
