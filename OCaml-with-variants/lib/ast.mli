(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string

type literal =
  | Int of int
  | Float of float
  | String of string

type arg =
  | Id of ident
  | Lit of literal

type binop =
  | AddInt
  | SubInt
  | MulInt
  | DivInt
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat

type exps =
  | Exp_fun of ident * exps (* arg + body *)
  | Exp_letbinding of ident * exps * exps option (* name + value + next exp *)
  | Exp_ident of ident
  | Exp_literal of literal
  | Exp_seq of exps * exps
  | Exp_apply of ident * exps list
  | Exp_binop of binop * exps * exps
  | Exp_ifthenelse of exps * exps * exps
  | Exp_unit

(* Application [f g ] *)
(** In type definition above the 3rd constructor is intentionally without documentation
to test linter *)
