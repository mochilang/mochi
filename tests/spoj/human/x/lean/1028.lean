/- Solution for SPOJ HUBULLU - Hubulullu
https://www.spoj.com/problems/HUBULLU/
-/

import Std
open Std

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let parts := (← h.getLine).trim.splitOn " "
    let s := parts[1]!.toNat! -- starting player: 0 for Airborne, 1 for Pagfloyd
    if s == 0 then
      IO.println "Airborne wins."
    else
      IO.println "Pagfloyd wins."
