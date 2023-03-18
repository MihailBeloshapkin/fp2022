(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string

type exp_type =
  | Rec
  | NonRec

type literal =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool

(** built-in binary operators *)
type binop =
  | AddInt
  | SubInt
  | MulInt
  | DivInt
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat
  | And
  | Or
  | LeqInt
  | GeqInt
  | EqInt

type exps =
  | Exp_fun of ident * exps (* arg + body *)
  | Exp_letbinding of exp_type * ident * exps * exps (* name + value + next exp *)
  | Exp_ident of ident
  | Exp_literal of literal
  | Exp_seq of exps * exps
  | Exp_apply of exps * exps list
  | Exp_binop of binop * exps * exps
  | Exp_ifthenelse of exps * exps * exps
  | Exp_match of exps * case list
  | Exp_polyvar of ident * exps list
  | Exp_unit

and case = exps * exps

type declaration = exp_type * string * exps

type top_level_expressions =
  | Declaration of declaration
  | Application of exps
