/- Solution for SPOJ WORDS1 - Play on Words
https://www.spoj.com/problems/WORDS1/
-/
import Std
open Std

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (root, parent') := find parent px
    (root, parent'.set! x root)

def union (parent : Array Nat) (a b : Nat) : Array Nat :=
  let (ra, parent1) := find parent a
  let (rb, parent2) := find parent1 b
  if ra = rb then parent2 else parent2.set! rb ra

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut indeg := Array.mkArray 26 0
    let mut outdeg := Array.mkArray 26 0
    let mut used := Array.mkArray 26 false
    let mut parent := Array.mkArray 26 0
    for i in [0:26] do
      parent := parent.set! i i
    for _ in [0:n] do
      let w := (← h.getLine).trim
      let chars := w.data
      let first := chars.head!
      let last := (chars.reverse).head!
      let a := first.toNat - 'a'.toNat
      let b := last.toNat - 'a'.toNat
      outdeg := outdeg.set! a (outdeg[a]! + 1)
      indeg := indeg.set! b (indeg[b]! + 1)
      used := used.set! a true
      used := used.set! b true
      parent := union parent a b
    let mut comp := 0
    let mut hasComp := false
    let mut connected := true
    for j in [0:26] do
      if used[j]! then
        let (root, parent') := find parent j
        parent := parent'
        if !hasComp then
          comp := root
          hasComp := true
        else if comp ≠ root then
          connected := false
    let mut start := 0
    let mut finish := 0
    for j in [0:26] do
      let diff : Int := (outdeg[j]! : Int) - (indeg[j]! : Int)
      if diff = 1 then
        start := start + 1
      else if diff = -1 then
        finish := finish + 1
      else if diff ≠ 0 then
        connected := false
    if connected && ((start = 1 && finish = 1) || (start = 0 && finish = 0)) then
      IO.println "Ordering is possible."
    else
      IO.println "The door cannot be opened."
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
