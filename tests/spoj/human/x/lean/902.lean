/- Solution for SPOJ HANGOVER - Hangover
https://www.spoj.com/problems/HANGOVER/
-/

import Std
open Std

partial def cardsNeeded (c : Float) : Nat :=
  let rec loop (sum : Float) (k : Nat) : Nat :=
    if sum >= c then k
    else loop (sum + 1.0 / Float.ofNat (k + 2)) (k + 1)
  loop 0.0 0

partial def process (h : IO.FS.Stream) (acc : Array String) : IO Unit := do
  let line ← h.getLine
  let t := line.trim
  if t = "0.00" then
    IO.println (String.intercalate "\n" acc)
  else
    let c := t.toFloat!
    let n := cardsNeeded c
    process h (acc.push s!"{n} card(s)")

def main : IO Unit := do
  process (← IO.getStdin) #[]
