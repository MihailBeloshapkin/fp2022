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
    
    let fdf f = f;;

    let firstmatcher x y f =
      match x with
      | 0 -> 1.0
      | 1 -> 2.0
      | 2 -> y
      | 3 -> f y
    ;;

    let matcher x = 
      let t = 
        match x with 
        | 0 -> 1 
        | 2 -> 0 
        | 5 -> fact 5 
        | v -> (incr v) * 30
      in 
      let v = 30 in
      v + t
    ;;

    let poly x =
      match x with
      | A -> 1
      | B -> 0
    ;;

    let listhead list =
      match list with
      | Cons(head, tail) -> Some(1)
      | Nil -> None
    ;;
    
    let listtail list =
      match list with
      | Cons(head, tail) -> Some(tail)
      | Nil -> None
    ;;

    let anothermatcher x =
      match x with
      | A -> 1
      | B -> 2
      | C (0) -> 3 
      | D ( E(v)) -> v
    ;;

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

(*
    let anothermatcher x =
      match x with
      | A -> 1
      | B -> 2
      | C (0) -> 3 
      | D (E(v)) -> v
    ;;

    let poly x =
      match x with
      | A -> 1
      | B -> 0
    ;;

    let listhead list =
      match list with
      | Cons(head, tail) -> Some(1)
      | Nil -> @none
    ;;

    let listtail list =
      match list with
      | Cons(head, tail) -> Some(tail)
      | Nil -> None
    ;;

    let optionid x =
      match x with
      | Some(v) -> Some(v)
      | None -> None
    ;;
    
      $ ./declAppl.exe <<-EOF
  > anothermatcher C(0) 
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

  $ ./declAppl.exe <<-EOF
  > anothermatcher D (E(31)) 
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

  $ ./declAppl.exe <<-EOF
  > D (1, 2, 4) 
  > EOF
  Value: D (1 2 4 )
  Not implemented: polyvar

  $ ./declAppl.exe <<-EOF
  > poly A
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

  $ ./declAppl.exe <<-EOF
  > listhead Cons(1, Cons(2, Cons(3, Nil))) 
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

  $ ./declAppl.exe <<-EOF
  > listtail Cons(1, Cons(2, Cons(3, Nil))) 
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

  $ ./declAppl.exe <<-EOF
  > optionid Some(1) 
  > EOF
  Fatal error: exception Not_found
  Raised at Stdlib__map.Make.find in file "map.ml", line 137, characters 10-25
  Called from Ocaml_with_var__Inter.Interpreter.get_from_env in file "lib/inter.ml" (inlined), line 81, characters 30-49
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 106, characters 32-51
  Called from Ocaml_with_var__Inter.Interpreter.eval in file "lib/inter.ml", line 111, characters 18-28
  Called from Dune__exe__DeclAppl in file "demos/declAppl.ml", line 60, characters 20-50
  [2]

    
    *)