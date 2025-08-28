/- Solution for SPOJ NEG2 - The Moronic Cowmpouter
https://www.spoj.com/problems/NEG2/
-/

import Std
open Std

partial def toNegBase2 (n : Int) : String :=
  if n == 0 then
    "0"
  else
    let rec loop (n : Int) (acc : List Char) : List Char :=
      if n == 0 then acc
      else
        let r := n % 2
        let q := (n - r) / (-2)
        let ch : Char := if r == 0 then '0' else '1'
        loop q (ch :: acc)
    String.mk (loop n [])

def main : IO Unit := do
  let line ← (← IO.getStdin).getLine
  let n : Int := line.trim.toInt!
  IO.println (toNegBase2 n)
