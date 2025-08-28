/- Solution for SPOJ PT07Y - Is it a tree?
https://www.spoj.com/problems/PT07Y/
-/

import Std
open Std

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, p2) := find parent px
    (r, p2.set! x r)

/-- Union two sets. Returns false if `a` and `b` were already connected. --/
def union (parent : Array Nat) (a b : Nat) : (Bool × Array Nat) :=
  let (ra, p1) := find parent a
  let (rb, p2) := find p1 b
  if ra = rb then
    (false, p2)
  else
    (true, p2.set! rb ra)

/-- Main program: reads graph and prints whether it is a tree. --/
def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
  if tokens.size < 2 then
    return
  let n := (tokens.get! 0).toNat!
  let m := (tokens.get! 1).toNat!
  if m ≠ n - 1 then
    IO.println "NO"
  else
    let mut parent : Array Nat := Array.mkArray (n + 1) 0
    for i in [0:n+1] do
      parent := parent.set! i i
    let mut ok := true
    let mut idx := 2
    for _ in [0:m] do
      let u := (tokens.get! idx).toNat!
      let v := (tokens.get! (idx + 1)).toNat!
      idx := idx + 2
      if ok then
        let (merged, p') := union parent u v
        parent := p'
        if !merged then
          ok := false
      else
        parent := parent
    if !ok then
      IO.println "NO"
    else
      let (root, p1) := find parent 1
      let mut parent2 := p1
      let mut connected := true
      for i in [2:n+1] do
        let (ri, p') := find parent2 i
        parent2 := p'
        if ri ≠ root then
          connected := false
      if connected then
        IO.println "YES"
      else
        IO.println "NO"
