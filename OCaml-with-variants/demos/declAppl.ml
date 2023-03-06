(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_with_var

(*let splitter = Str.split (Str.regexp ";;")
*)
let () =
  let open Ocaml_with_var.Parser in
  (* let s = Stdio.In_channel.input_all Caml.stdin in *)
  let s0 = "let rec fact x = if x < 2 then 1 else x * (fact (x - 1));;" in
  let s = Stdio.In_channel.input_all Caml.stdin in
  match parse_exp s0 with
  | Result.Ok result ->
    (match result with
     | Declaration (_, name, decl_expr) ->
       (match parse_exp s with
        | Result.Ok (Application exps) ->
          let t = Repl.try_infer decl_expr in
          let result =
            Inter.Interpreter.eval [ name, Repl.get_exp_type decl_expr ] exps
          in
          Repl.output result;
          Caml.Format.printf "\nType: ";
          Repl.print_result_of_inference t;
        | Result.Ok (Declaration (_, _, _)) -> Format.printf "Unexpected declaration"
        | Error msg -> Format.printf "Some error  : %s" msg)
     | Application exps ->
       let result = Inter.Interpreter.eval [] exps in
       Repl.output result)
  | Error msg -> Format.printf "Some error: %s" msg
;;
