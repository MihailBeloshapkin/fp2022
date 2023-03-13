Copyright 2021-2022, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned


  $ ./declAppl.exe <<-EOF
  > fact 
  > EOF
  Value: Function
  Type: int ->int 

  $ ./declAppl.exe <<-EOF
  > fix 
  > EOF
  Value: Function
  Type: 2 ->5 ->2 ->5 ->2 ->5 

  $ ./declAppl.exe <<-EOF
  > selffact 
  > EOF
  Value: Function
  Type: int ->int ->int ->int 

  $ ./declAppl.exe <<-EOF
  > fixfact
  > EOF
  Value: Function
  Type: int ->int 

  $ ./declAppl.exe <<-EOF
  > fixfact 5 
  > EOF
  Value: 120
  Type: int 

  $ ./declAppl.exe <<-EOF
  > fact 5
  > EOF
  Value: 120
  Type: int 

  $ ./declAppl.exe <<-EOF
  > (fact 5) + (fact 7)
  > EOF
  Value: 5160
  Type: int 

  $ ./declAppl.exe <<-EOF
  > (fact (fact 3))
  > EOF
  Value: 720
  Type: int 

  $ ./declAppl.exe <<-EOF
  > (fact 5) * (incr 30)
  > EOF
  Value: 3720
  Type: int 

  $ ./declAppl.exe <<-EOF
  > (fact 5) = 120
  > EOF
  Value: true
  Type: bool 

  $ ./declAppl.exe <<-EOF
  > add (fact 5) (incr 2 + 30)
  > EOF
  Value: 153
  Type: int 

  $ ./declAppl.exe <<-EOF
  > (fun x -> x + 1) 5 
  > EOF
  Value: 6
  Type: int 

  $ ./declAppl.exe <<-EOF
  > matcher 30 
  > EOF
  Value: 960
  Type infetence failed

  $ ./declAppl.exe <<-EOF
  > firstmatcher 0 5.0 (fun x -> x + 5.0)
  > EOF
  Value: 1.000000
  Type infetence failed

  $ ./declAppl.exe <<-EOF
  > firstmatcher 
  > EOF
  Value: Function
  Type: int ->float ->float ->float ->float 

  $ ./declAppl.exe <<-EOF
  > (fun a b -> a + b) 
  > EOF
  Value: Function
  Type: int ->int ->int 

  $ ./declAppl.exe <<-EOF
  > (fun a b -> a + b) 1 
  > EOF
  Value: Function
  Type: int ->int 

  $ ./declAppl.exe <<-EOF
  > (fun a b -> a + b) 1 2 
  > EOF
  Value: 3
  Type: int 

  $ ./declAppl.exe <<-EOF
  > fdf (fun x -> x) 
  > EOF
  Value: Function
  Type infetence failed

  $ ./declAppl.exe <<-EOF
  > fcf (fun a b -> a + b) 3 4 
  > EOF
  Value: 7
  Type infetence failed
