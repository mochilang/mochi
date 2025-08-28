/- Solution for SPOJ CHAIN - Strange Food Chain
https://www.spoj.com/problems/CHAIN/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Find with path compression, returning updated parent and relation arrays. --/
partial def find (parent rel : Array Nat) (x : Nat) :
  Nat × Array Nat × Array Nat :=
  let px := parent.get! x
  if px = x then
    (x, parent, rel)
  else
    let (r, parent', rel') := find parent rel px
    let rx := (rel.get! x + rel'.get! px) % 3
    let parent'' := parent'.set! x r
    let rel'' := rel'.set! x rx
    (r, parent'', rel'')

/-- Merge two nodes with required difference `d` (0: same, 1: x eats y). Returns updated arrays and whether the statement is consistent. --/
def merge (parent rel : Array Nat) (x y d : Nat) :
  Array Nat × Array Nat × Bool :=
  let (rx, parent1, rel1) := find parent rel x
  let (ry, parent2, rel2) := find parent1 rel1 y
  let dx := rel2.get! x
  let dy := rel2.get! y
  if rx = ry then
    let ok := ((dx + 3 - dy) % 3) = d
    (parent2, rel2, ok)
  else
    let diff := (d + dy + 3 - dx) % 3
    let parent3 := parent2.set! rx ry
    let rel3 := rel2.set! rx diff
    (parent3, rel3, true)

/-- Solve a single test case starting at `start` in `data`. --/
partial def solveCase (data : Array Nat) (start : Nat) :
  (Nat × Nat) := Id.run do
  let n := data.get! start
  let k := data.get! (start + 1)
  let mut idx := start + 2
  let mut parent : Array Nat := Array.mkArray (n + 1) 0
  let mut rel : Array Nat := Array.mkArray (n + 1) 0
  for i in [0:n+1] do
    parent := parent.set! i i
    rel := rel.set! i 0
  let mut bad := 0
  for _ in [0:k] do
    let d := data.get! idx; let x := data.get! (idx + 1); let y := data.get! (idx + 2)
    idx := idx + 3
    if x > n || y > n then
      bad := bad + 1
    else if d = 2 && x = y then
      bad := bad + 1
    else
      let diff := if d = 1 then 0 else 1
      let (parent', rel', ok) := merge parent rel x y diff
      parent := parent'; rel := rel'
      if !ok then bad := bad + 1
  (bad, idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then
    return
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (ans, idx') := solveCase data idx
    IO.println ans
    idx := idx'
