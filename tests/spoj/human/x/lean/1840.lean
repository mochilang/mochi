/- Solution for SPOJ PQUEUE - Printer Queue
https://www.spoj.com/problems/PQUEUE/
-/

import Std
open Std

def parseInts (s : String) : List Nat :=
  s.trim.split (· = ' ') |>.filter (fun t => t.length > 0) |>.map (·.toNat!)

partial def simulate : List (Nat × Nat) -> Nat -> Nat -> Nat
| [], _, count => count
| (idx, p) :: rest, target, count =>
    let hasHigher := rest.any (fun (_, q) => q > p)
    if hasHigher then
      simulate (rest ++ [(idx, p)]) target count
    else
      let count := count + 1
      if idx = target then count else simulate rest target count

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let nm := parseInts line
    let n := nm.head!
    let m := nm.get! 1
    let prLine ← h.getLine
    let priorities := parseInts prLine
    let queue := (List.range n).zip priorities
    IO.println (simulate queue m 0)
    solve h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
