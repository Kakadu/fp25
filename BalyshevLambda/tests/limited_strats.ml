(* TODO (psi) : make cram tests instead of it *)

open Lambda_lib

let parse_optimistically str = Result.get_ok (Parser.parse str)

let pp_limited (expr, lim) =
  match lim with
  | Lambda.Exhausted -> Format.printf "Partial: %a" Printast.pp_named expr
  | Lambda.Limited lim ->
    Format.printf "Evaluated! Reductions left: %d\nResult: %a" lim Printast.pp_named expr
  | Lambda.Unlimited -> Format.printf "Evaluated!\nResult: %a" Printast.pp_named expr
;;

let eval_ao_limited input lim =
  Lambda.apply_limited_strat
    Lambda.ao_limited
    (parse_optimistically input)
    (Lambda.Limited lim)
;;

let%expect_test _ =
  eval_ao_limited "λx.x" 1 |> pp_limited;
  [%expect {|
    Evaluated! Reductions left: 1
    Result: (Abs (x, (Var x))) |}]
;;

let%expect_test _ =
  eval_ao_limited "(λx.x) a" 1 |> pp_limited;
  [%expect {|
    Evaluated! Reductions left: 0
    Result: (Var a) |}]
;;

let%expect_test _ =
  eval_ao_limited "(λx.λy.x y) a b" 1 |> pp_limited;
  [%expect {|
    Partial: (App ((Abs (y, (App ((Var a), (Var y))))), (Var b))) |}]
;;

let%expect_test _ =
  eval_ao_limited "(λx.λy.λz.x y z) a b c" 1 |> pp_limited;
  [%expect
    {|
    Partial: (App (
                (App (
                   (Abs (y, (Abs (z, (App ((App ((Var a), (Var y))), (Var z)))))
                      )),
                   (Var b))),
                (Var c))) |}]
;;

let eval_cbn_limited input lim =
  Lambda.apply_limited_strat
    Lambda.cbn_limited
    (parse_optimistically input)
    (Lambda.Limited lim)
;;

let%expect_test _ =
  eval_cbn_limited "(λx.x) a" 1 |> pp_limited;
  [%expect {|
    Evaluated! Reductions left: 0
    Result: (Var a) |}]
;;

let%expect_test _ =
  eval_cbn_limited "(λx.λy.x y) a b" 1 |> pp_limited;
  [%expect {|
    Partial: (App ((Abs (y, (App ((Var a), (Var y))))), (Var b))) |}]
;;

let%expect_test _ =
  eval_cbn_limited "(λx.λy.x y) a b" 2 |> pp_limited;
  [%expect {|
    Evaluated! Reductions left: 0
    Result: (App ((Var a), (Var b))) |}]
;;

let%expect_test _ =
  eval_cbn_limited "(λy.(λx.x) a)" 10 |> pp_limited;
  [%expect
    {|
    Evaluated! Reductions left: 10
    Result: (Abs (y,
                                              (App ((Abs (x, (Var x))), (Var a)))
                                              )) |}]
;;

let%expect_test _ =
  eval_cbn_limited "(λx.x x) (λx.x x)" 100 |> pp_limited;
  [%expect
    {|
    Evaluated! Reductions left: 99
    Result: (App (
                                              (Abs (x, (App ((Var x), (Var x))))),
                                              (Abs (x, (App ((Var x), (Var x)))))
                                              )) |}]
;;
