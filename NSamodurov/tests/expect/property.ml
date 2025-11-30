open Lambda_lib
open Parser
open Ast

let rec shrink =
  let open QCheck.Iter in
  let open QCheck.Shrink in
  function
  | EConst (Int i) -> int i >|= eint
  | EConst (Bool i) -> bool i >|= ebool
  | EVar i -> string i >|= evar
  | ELet (flag, v, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink e1 >|= fun e1' -> elet flag v e1' e2)
    <+> (shrink e2 >|= fun e2' -> elet flag v e1 e2')
  | EIf (p, t, e) ->
    of_list [ p; t; e ]
    <+> (shrink p >|= fun p' -> eif p' t e)
    <+> (shrink t >|= fun t' -> eif p t' e)
    <+> (shrink e >|= fun e' -> eif p t e')
  | EAbs (v, e) -> of_list [ e ] <+> (shrink e >|= fun e' -> eabs v e')
  | EApp (e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink e1 >|= fun e1' -> eapp e1' e2)
    <+> (shrink e2 >|= fun e2' -> eapp e1 e2')
;;

let varname =
  QCheck.Gen.map
    (fun x -> String.make 1 (Char.chr x))
    (QCheck.Gen.int_range (Char.code 'a') (Char.code 'z'))
;;

let gen =
  let open QCheck.Gen in
  sized
    (fix (fun _self -> function
       | 0 ->
         frequency
           [ 1, map evar varname
           ; 1, map eint (fun x -> abs (QCheck.Gen.int x))
           ; 1, map ebool QCheck.Gen.bool
           ]
       | n ->
         frequency
           [ 1, map2 eabs (fun _x -> "var") (_self (n / 2))
           ; 1, map2 eapp (_self (n / 2)) (_self (n / 2))
           ; 1, map3 eif (_self (n / 3)) (_self (n / 3)) (_self (n / 3))
           ; ( 1
             , map4
                 elet
                 (fun _x -> Recursive)
                 (fun _x -> "var")
                 (_self ((n / 2) - 1))
                 (_self ((n / 2) - 1)) )
           ]))
;;

let arbitrary = QCheck.make gen ~print:(Format.asprintf "%a" Pprintast.pp) ~shrink

let run () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary (fun l ->
          match
            Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              (parse_lam.apps parse_lam)
              (Format.asprintf "%a" Pprintast.pp l)
          with
          | Result.Ok after when after = l -> true
          | Result.Ok _after ->
            Format.printf "before : %a\n%!" Pprintast.pp l;
            Format.printf "       : `%a`\n%!" Pprintast.pp l;
            Format.printf "`%a`\n%!" Pprintast.pp _after;
            false
          | Result.Error _ ->
            Format.printf "failed on : %a\n%!" Pprintast.pp l;
            false))
    ]
;;

let () =
  let _i = run () in
  ()
;;
