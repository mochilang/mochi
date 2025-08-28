/- Solution for SPOJ NGM - A Game with Numbers
https://www.spoj.com/problems/NGM/
-/
import Std
open Std

def main : IO Unit := do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  let n := line.trim.toNat!
  if n % 10 == 0 then
    IO.println "2"
  else
    IO.println "1"
    IO.println (n % 10)
