/- Solution for SPOJ TOUR - Fake tournament
https://www.spoj.com/problems/TOUR/
-/

import Std
open Std

private def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ') |>.filter (· ≠ "") |>.map String.toNat!

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut adj : Array (Array Nat) := Array.mkArray n #[]
    for i in [0:n] do
      let nums := parseInts (← h.getLine)
      match nums with
      | _m :: rest =>
        for w in rest do
          adj := adj.modify (w-1) (·.push i)
      | [] => pure ()
    let mut count := 0
    for i in [0:n] do
      let mut reach : Array Bool := Array.mkArray n false
      for j in adj.get! i do
        reach := reach.set! j true
      for j in adj.get! i do
        for k in adj.get! j do
          reach := reach.set! k true
      let mut ok := true
      for j in [0:n] do
        if i ≠ j && !(reach.get! j) then
          ok := false
      if ok then
        count := count + 1
    IO.println count
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
