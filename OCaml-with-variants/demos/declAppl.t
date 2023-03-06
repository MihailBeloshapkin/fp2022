Copyright 2021-2022, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./declAppl.exe <<-EOF
  > fact 5
  > EOF
  120

  $ ./declAppl.exe <<-EOF
  > (fact 5) + (fact 7)
  > EOF
  5160
  
  $ ./declAppl.exe <<-EOF
  > (fact (fact 3))
  > EOF
  720
  