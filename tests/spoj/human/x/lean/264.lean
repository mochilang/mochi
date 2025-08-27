/- Solution for SPOJ CORNET - Corporative Network
https://www.spoj.com/problems/CORNET/
-/

import Std
open Std

partial def find (parent : Array Nat) (dist : Array Nat) (x : Nat) :
  (Nat × Array Nat × Array Nat) :=
  let p := parent[x]!
  if p = x then
    (x, parent, dist)
  else
    let dx := dist[x]!
    let (root, parent', dist') := find parent dist p
    let total := dx + dist'[p]!
    let parent'' := parent'.set! x root
    let dist'' := dist'.set! x total
    (root, parent'', dist'')

partial def processCommands (toks : Array String) (idx : Nat)
    (parent dist : Array Nat) : IO (Nat × Array Nat × Array Nat) := do
  let cmd := toks[idx]!
  if cmd = "O" then
    return (idx + 1, parent, dist)
  else if cmd = "E" then
    let i := (toks[idx+1]!).toNat!
    let (_, parent', dist') := find parent dist i
    let parent := parent'
    let dist := dist'
    IO.println (dist[i]!)
    processCommands toks (idx + 2) parent dist
  else
    let i := (toks[idx+1]!).toNat!
    let j := (toks[idx+2]!).toNat!
    let diff := Int.natAbs (Int.ofNat i - Int.ofNat j) % 1000
    let parent := parent.set! i j
    let dist := dist.set! i diff
    processCommands toks (idx + 3) parent dist

partial def solveCases (toks : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (toks[idx]!).toNat!
    let mut parent : Array Nat := Array.replicate (n+1) 0
    let mut dist : Array Nat := Array.replicate (n+1) 0
    for i in [0:n+1] do
      parent := parent.set! i i
      dist := dist.set! i 0
    let (idx', _, _) ← processCommands toks (idx + 1) parent dist
    solveCases toks idx' (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := (data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "")).toArray
  let t := (toks[0]!).toNat!
  solveCases toks 1 t
