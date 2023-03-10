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
  > fixfact 
  > EOF
  Value: Function
  Type: int ->int ->int ->int 

  $ ./declAppl.exe <<-EOF
  > f 
  > EOF
  Value: Function
  Type: int ->int 

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
