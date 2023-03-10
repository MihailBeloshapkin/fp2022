(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Infer
open Inter
open Parser
open Inter.Interpreter.ContextData

let print_t = print_sig

let print_result_of_inference = function
  | Result.Ok res ->
    Caml.Format.printf "\nType: ";
    print_t res
  | _ -> Caml.Format.printf "\nType infetence failed"
;;

let get_exp_type = function
  | Exp_fun _ as f -> Func ([], f)
  | Exp_literal (Int i) -> Int i
  | Exp_literal (Float f) -> Float f
  | _ -> Undefined
;;

let output = function
  | Ok (Interpreter.ContextData.Int i) -> Caml.Format.printf "%i" i
  | Ok (Interpreter.ContextData.Float f) -> Caml.Format.printf "%f" f
  | Ok (Interpreter.ContextData.String s) -> Caml.Format.printf "%s" s
  | Ok (Interpreter.ContextData.Bool b) -> Caml.Format.printf "%b" b
  | Ok (Interpreter.ContextData.Func _) -> Caml.Format.printf "Function"
  | _ -> Caml.Format.printf "FAILED"
;;

let read_next prev =
  let input = read_line () in
  input ^ prev
;;

(** Load source code from the file. *)
let get_file filename =
  let current_channel = open_in filename in
  let data = really_input_string current_channel (in_channel_length current_channel) in
  close_in current_channel;
  data
;;

open Caml.Format

let read_code_from_file path =
  try
    let declaration_list = path |> get_file |> parse_several_declarations in
    match declaration_list with
    | Result.Ok funcs ->
      List.fold
        ~f:
          (fun acc -> function
            | Declaration (_, name, expr) -> (name, get_exp_type expr) :: acc
            | _ -> acc)
        ~init:[]
        funcs
    | Result.Error msg ->
      printf "Incorrect file content: %s" msg;
      []
  with
  | _ ->
    printf "%s" path;
    []
;;

let read_several_declarations code =
  let declaration_list = code |> parse_several_declarations in
  match declaration_list with
  | Result.Ok funcs ->
    funcs
    (* List.fold ~f:(fun acc -> function Declaration (_, name, expr) -> (name, get_exp_type expr) :: acc | _ -> acc) ~init:[] funcs *)
  | Result.Error msg ->
    printf "Incorrect file content: %s" msg;
    []
;;

let to_ctx =
  List.fold
    ~f:
      (fun acc -> function
        | Declaration (_, name, expr) -> (name, get_exp_type expr) :: acc
        | _ -> acc)
    ~init:[]
;;
