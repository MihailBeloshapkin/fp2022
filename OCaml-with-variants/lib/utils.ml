(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

(*
let map_ast ast =
  let rec sub t =
    match t with
    | Exp_fun (name, i, exps) -> Exp_fun (name, i, sub exps)
    | Exp_letbinding (i, exps) -> Exp_letbinding (i, sub exps)
    | Exp_ident _ as i -> i
    | Exp_literal _ as l -> l
    | Exp_seq (e1, e2) -> Exp_seq (sub e1, sub e2)
    | Exp_apply ("+", [Exp_literal (Int op1); Exp_literal (Int op2)]) -> Exp_binop (AddInt (op1, op2))
    | Exp_apply ("-", [Exp_literal (Int op1); Exp_literal (Int op2)]) -> Exp_binop (SubInt (op1, op2))
    | _ as i -> i
  in
  sub ast*)
