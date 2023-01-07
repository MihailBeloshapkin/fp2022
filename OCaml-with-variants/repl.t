
  $ ./REPL.exe
  ./REPL.exe: not found
  [127]
  $ ./REPL.exe -help
  ./REPL.exe: not found
  [127]
  $ ./REPL.exe -cbv - <<EOF
  > \f.x
  ./REPL.exe: not found
  [127]
  $ ./REPL.exe -no - <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  ./REPL.exe: not found
  [127]
