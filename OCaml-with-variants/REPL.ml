open Base
open Ocaml_with_var.Repl
open Ocaml_with_var.Infer

let run_repl _ =
  Caml.Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"
;;

(*
let run_single eval =
  let open Lambda_lib in
  let text = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Caml.Format.printf "Error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    Caml.Format.printf "Parsed result: %a\n%!" Printast.pp_named ast;
    (match eval ast with
     | rez -> Caml.Format.printf "Evaluated result: %a\n%!" Printast.pp_named rez)
;;
*)

type strategy =
  | CBN
  | CBV
  | NO
  | AO

type opts =
  { mutable batch : bool
  ; mutable stra : strategy
  }

let () =
  let opts = { batch = false; stra = CBN } in
  let open Caml.Arg in
  parse
    [ ( "-"
      , Unit (fun () -> opts.batch <- true)
      , "Read from stdin single program, instead of running full REPL" )
    ; "-cbv", Unit (fun () -> opts.stra <- CBV), "Call-by-value strategy"
    ; "-cbn", Unit (fun () -> opts.stra <- CBN), "Call-by-name strategy"
    ; "-no", Unit (fun () -> opts.stra <- NO), "Normal Order strategy"
    ; "-ao", Unit (fun () -> opts.stra <- NO), "Applicative Order strategy"
    ]
    (fun _ ->
      Caml.Format.eprintf "Positioned arguments are not supported\n";
      Caml.exit 1)
    "Read-Eval-Print-Loop for Utyped Lambda Calculus"
;;

let try_infer e = infer e TypeEnv.empty

let ex1 =
  try_infer
    (Exp_ifthenelse
       ( Exp_literal (Bool true)
       , Exp_binop (MulFloat, Exp_literal (Float 1.), Exp_literal (Float 0.))
       , Exp_binop (AddFloat, Exp_literal (Float 31.), Exp_literal (Float 0.)) ))
;;

let ex2 = 
  try_infer
    (Exp_fun ("n", Exp_ifthenelse ( Exp_ident "n", Exp_literal (Int 1), Exp_literal (Int 30))))
;;

let ex3 = 
  try_infer
    (Exp_fun ("n", Exp_ifthenelse ( Exp_binop (LeqInt, Exp_ident "n", Exp_literal (Int 2)), Exp_literal (Int 1), Exp_binop (MulInt, Exp_ident "n", Exp_literal (Int 30)))))
;;

let ex4 = 
  try_infer
    (Exp_fun ("n", Exp_fun ("m", Exp_binop (AddInt, Exp_ident "m", Exp_ident "n"))))
;;

let ex5 =
  try_infer
    (Exp_letbinding (NonRec, "f", (Exp_fun ("n", Exp_fun ("m", Exp_fun ("k", Exp_ident "n")))), Exp_apply ("f", [ Exp_literal (Float 30.) ])))

let ex7 = 
  try_infer

let () =
  let res =
    try_infer
      (Exp_letbinding (Rec, "a", Exp_literal (Float 4.), 
      Exp_letbinding (Rec, "b", Exp_literal (Float 5.), Exp_binop (AddFloat, Exp_ident "a", Exp_ident "b"))))
  in
  match ex5 with
  | Result.Ok res -> print_sig res;
  | _ -> Caml.Format.printf "Failed inference"
;;
(*
let _ = run0 "let fact x = if x < 2 then 1 else x * (fact (x - 1));;" "fact 5"*)
