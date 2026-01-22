(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier =
  (string
  [@gen
    QCheck.Gen.string_size
      ~gen:(QCheck.Gen.char_range 'a' 'z')
      (QCheck.Gen.int_range 5 10)])
[@@deriving eq, qcheck]

type integer = (int[@gen QCheck.Gen.int_range 0 1024]) [@@deriving eq, qcheck]

type mlterm =
  | Var of identifier
  | Int of integer
  | Bool of bool
  | Unit
  | ITE of mlterm * mlterm * mlterm
  | Let of identifier * mlterm * mlterm
  | LetRec of identifier * mlterm * mlterm
  | App of mlterm * mlterm
  | Fun of identifier * mlterm
[@@deriving eq, qcheck]
