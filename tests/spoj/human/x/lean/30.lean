/- Solution for SPOJ BLINNET - Bytelandian Blingors Network
https://www.spoj.com/problems/BLINNET/
-/

import Std
open Std

structure Edge where
  u : Nat
  v : Nat
  w : Nat

partial def readNonEmpty (h : IO.FS.Stream) : IO String := do
  let line ← h.getLine
  let s := line.trim
  if s.isEmpty then readNonEmpty h else pure s

partial def readNat (h : IO.FS.Stream) : IO Nat :=
  return (← readNonEmpty h).toNat!

partial def readEdges (h : IO.FS.Stream) (n : Nat) : IO (Array Edge) := do
  let mut edges : Array Edge := #[]
  for i in [1:n+1] do
    discard (← readNonEmpty h) -- city name
    let p := (← readNonEmpty h).toNat!
    for _ in [0:p] do
      let line := (← readNonEmpty h)
      let parts := line.splitOn " "
      let neigh := parts.get! 0 |>.toNat!
      let cost := parts.get! 1 |>.toNat!
      edges := edges.push {u := i, v := neigh, w := cost}
  return edges

partial def find (parent : IO.Ref (Array Nat)) (x : Nat) : IO Nat := do
  let p ← parent.get
  let px := p.get! x
  if px = x then
    return x
  else
    let root ← find parent px
    let p2 ← parent.get
    parent.set (p2.set! x root)
    return root

partial def union (parent rank : IO.Ref (Array Nat)) (a b : Nat) : IO Bool := do
  let ra ← find parent a
  let rb ← find parent b
  if ra = rb then
    return False
  let pr ← parent.get
  let rk ← rank.get
  let rka := rk.get! ra
  let rkb := rk.get! rb
  if rka < rkb then
    parent.set (pr.set! ra rb)
    return True
  else if rkb < rka then
    parent.set (pr.set! rb ra)
    return True
  else
    parent.set (pr.set! rb ra)
    rank.set (rk.set! ra (rka + 1))
    return True

partial def mst (n : Nat) (edges : Array Edge) : IO Nat := do
  let parent ← IO.mkRef (Array.init (n+1) id)
  let rank ← IO.mkRef (Array.mkArray (n+1) 0)
  let mut total := 0
  let mut taken := 0
  let sorted := edges.qsort (fun a b => a.w < b.w)
  for e in sorted do
    if taken < n - 1 then
      if ← union parent rank e.u e.v then
        total := total + e.w
        taken := taken + 1
  return total

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    return
  else
    let n := (← readNat h)
    let edges ← readEdges h n
    let res ← mst n edges
    IO.println res
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← readNat h)
  process h t
