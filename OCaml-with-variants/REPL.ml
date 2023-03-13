open Base
open Ocaml_with_var.Repl
open Ocaml_with_var.Infer
open Ocaml_with_var.Parser

let run_repl _ =
  Caml.Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"
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
    (Exp_fun
       ("n", Exp_ifthenelse (Exp_ident "n", Exp_literal (Int 1), Exp_literal (Int 30))))
;;

let ex3 =
  try_infer
    (Exp_fun
       ( "n"
       , Exp_ifthenelse
           ( Exp_binop (LeqInt, Exp_ident "n", Exp_literal (Int 2))
           , Exp_literal (Int 1)
           , Exp_binop (MulInt, Exp_ident "n", Exp_literal (Int 30)) ) ))
;;

let ex4 =
  try_infer
    (Exp_fun ("n", Exp_fun ("m", Exp_binop (AddInt, Exp_ident "m", Exp_ident "n"))))
;;

let ex5 =
  try_infer
    (Exp_letbinding
       ( NonRec
       , "f"
       , Exp_fun ("n", Exp_fun ("m", Exp_binop (AddInt, Exp_ident "m", Exp_ident "n")))
       , Exp_apply (Exp_ident "f", [ Exp_literal (Int 30); Exp_literal (Int 40) ]) ))
;;

let ex7 =
  try_infer
    (Exp_letbinding
       ( Rec
       , "f"
       , Exp_fun
           ( "n"
           , Exp_ifthenelse
               ( Exp_binop (EqInt, Exp_literal (Int 1), Exp_ident "n")
               , Exp_literal (Int 1)
               , Exp_apply
                   ( Exp_ident "f"
                   , [ Exp_binop (SubInt, Exp_ident "n", Exp_literal (Int 1)) ] ) ) )
       , Exp_ident "f" ))
;;

let ex8 =
  try_infer
    (Exp_letbinding
       ( Rec
       , "fact"
       , Exp_fun
           ( "n"
           , Exp_ifthenelse
               ( Exp_binop (LeqInt, Exp_ident "n", Exp_literal (Int 2))
               , Exp_literal (Int 1)
               , Exp_binop
                   ( MulInt
                   , Exp_ident "n"
                   , Exp_apply
                       ( Exp_ident "fact"
                       , [ Exp_binop (SubInt, Exp_ident "n", Exp_literal (Int 1)) ] ) ) )
           )
       , Exp_ident "fact" ))
;;

let () =
  match ex8 with
  | Result.Ok res -> print_sig res
  | _ -> Caml.Format.printf "Failed inference"
;;
