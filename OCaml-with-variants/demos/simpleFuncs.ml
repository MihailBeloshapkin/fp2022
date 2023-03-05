(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_with_var

let () =
  let open Ocaml_with_var.Parser in
  let s = Stdio.In_channel.input_all Caml.stdin in
  match parse_exp s with
  | Result.Ok result ->
    (match result with
     | Declaration _ -> failwith "err"
     | Application exps ->
       let result = Inter.Interpreter.eval [] exps in
       Repl.output result)
  | Error _ -> Format.printf "Some error"
;;
