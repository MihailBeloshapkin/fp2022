(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_with_var

let () =
  let open Ocaml_with_var.Parser in
  let ctx =
    Utils.read_several_declarations
      {|
    let rec fix f x = f (fix f) x;;
    let rec fact x = if x < 2 then 1 else x * (fact (x - 1));;
    let selffact self n = if n = 0 then 1 else n * self (n - 1);;
    let fixfact n = fix selffact n;;
    let incr x = x + 1;;
    let add x y = x + y;;
    let fcf f x y = f x y;;
  |}
  in
  let env = Repl.infer_declaration_list ctx in
  let ctx = Utils.to_ctx ctx in
  let s = Stdio.In_channel.input_all Caml.stdin in
  match parse_exp s with
  | Result.Ok exp_ast ->
    (match exp_ast with
     | Declaration _ -> Format.printf "Application expected"
     | Application exp as appl ->
       let open Caml.Format in
       let typed = Infer.infer_top_level_expressions env appl in
       let result = Inter.Interpreter.eval ctx exp in
       printf "Value: ";
       Utils.output result;
       Utils.print_result_of_inference typed)
  | Error msg -> Format.printf "Some error: %s" msg
;;
