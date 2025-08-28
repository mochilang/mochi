/- Solution for SPOJ STEAD - Steady Cow Assignment
https://www.spoj.com/problems/STEAD/
-/

import Std
open Std

-- Edmonds-Karp max flow on a capacity matrix
private def maxFlow (capInit : Array (Array Nat)) (s t : Nat) : Nat :=
  Id.run do
    let n := capInit.size
    let mut cap := capInit
    let mut total := 0
    let inf := 1000000
    while true do
      let mut parent : Array (Option Nat) := Array.replicate n none
      parent := parent.set! s (some s)
      let mut q : Array Nat := #[s]
      let mut qi := 0
      while qi < q.size && parent[t]! = none do
        let u := q[qi]!; qi := qi + 1
        for v in [0:n] do
          if parent[v]! = none && cap[u]![v]! > 0 then
            parent := parent.set! v (some u)
            q := q.push v
      if parent[t]! = none then
        break
      let mut f := inf
      let mut v := t
      while v ≠ s do
        let u := (parent[v]!).get!
        f := Nat.min f cap[u]![v]!
        v := u
      total := total + f
      v := t
      while v ≠ s do
        let u := (parent[v]!).get!
        let rowu := cap[u]!
        cap := cap.set! u (rowu.set! v (rowu[v]! - f))
        let rowv := cap[v]!
        cap := cap.set! v (rowv.set! u (rowv[u]! + f))
        v := u
    return total

-- Check if interval [l,r] of ranks allows assigning all cows
private def feasible (prefs : Array (Array Nat)) (caps : Array Nat) (l r : Nat) : Bool :=
  Id.run do
    let n := prefs.size
    let b := caps.size
    let size := n + b + 2
    let src := 0
    let sink := size - 1
    let mut cap : Array (Array Nat) := Array.replicate size (Array.replicate size 0)
    -- source to cows
    for i in [0:n] do
      let row := cap[src]!
      cap := cap.set! src (row.set! (1 + i) 1)
    -- cows to barns within range
    for i in [0:n] do
      let rowCow := cap[1+i]!
      let mut row2 := rowCow
      for pos in [l:r+1] do
        let barn := prefs[i]![pos]!
        row2 := row2.set! (1 + n + barn) 1
      cap := cap.set! (1 + i) row2
    -- barns to sink
    for j in [0:b] do
      let row := cap[1+n+j]!
      cap := cap.set! (1+n+j) (row.set! sink caps[j]!)
    let f := maxFlow cap src sink
    return f = n

-- Compute minimal range
private def minRange (prefs : Array (Array Nat)) (caps : Array Nat) : Nat :=
  let b := caps.size
  Id.run do
    for w in [1:b+1] do
      let mut l := 0
      while l + w - 1 < b do
        if feasible prefs caps l (l + w - 1) then
          return w
        l := l + 1
    return b

-- Read all integers from stdin
private def readInts : IO (Array Nat) := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
  return toks.map String.toNat!

-- Main program
def main : IO Unit := do
  let arr ← readInts
  if arr.size = 0 then return ()
  let n := arr[0]!
  let b := arr[1]!
  let mut idx := 2
  let mut prefs : Array (Array Nat) := Array.replicate n (Array.replicate b 0)
  for i in [0:n] do
    let mut row := Array.replicate b 0
    for j in [0:b] do
      row := row.set! j (arr[idx]! - 1)
      idx := idx + 1
    prefs := prefs.set! i row
  let mut caps : Array Nat := Array.replicate b 0
  for j in [0:b] do
    caps := caps.set! j arr[idx]!
    idx := idx + 1
  let ans := minRange prefs caps
  IO.println ans
