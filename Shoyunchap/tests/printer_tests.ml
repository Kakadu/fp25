(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib
open Parser
open Printer

let samples : Ast.expression list =
  [ Ast.Const (Ast.Int 42)
  ; Ast.Var "x"
  ; Ast.App (Ast.Var "f", Ast.Const (Ast.Int 1))
  ; Ast.Fun ("x", Ast.Const (Ast.Int 0))
  ; Ast.BinOp (Ast.OpMul, Ast.Const (Ast.Int 2), Ast.Const (Ast.Int 3))
  ; Ast.Let
      ( Ast.LocalVar
      , Ast.NonRec
      , "y"
      , Ast.Const (Ast.Int 7)
      , Some (Ast.BinOp (Ast.OpAdd, Ast.Var "y", Ast.Const (Ast.Int 1))) )
  ; Ast.If
      (Ast.Const (Ast.Int 0), Ast.Const (Ast.Int 1), Some (Ast.Const (Ast.Int 2)))
  ]

let () =
  List.iter
    (fun e ->
      let first = string_of_expr e in
      match parse first with
      | Error _ -> failwith ("parse failed on: " ^ first)
      | Ok e' ->
          let second = string_of_expr e' in
          match parse second with
          | Error _ -> failwith ("roundtrip parse failed on: " ^ second)
          | Ok e'' ->
              let third = string_of_expr e'' in
              if not (String.equal second third) then
                failwith ("printer is not stable on: " ^ first))
    samples
