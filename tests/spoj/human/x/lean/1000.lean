/- Solution for SPOJ EQDIV - Equidivisions
https://www.spoj.com/problems/EQDIV/
-/

import Std
open Std

def neighbors (idx n : Nat) : List Nat :=
  let r := idx / n
  let c := idx % n
  let res := if r > 0 then [idx - n] else []
  let res := if r + 1 < n then (idx + n) :: res else res
  let res := if c > 0 then (idx - 1) :: res else res
  let res := if c + 1 < n then (idx + 1) :: res else res
  res

def bfsCount (n : Nat) (grid : Array Nat) (area start : Nat) : Nat :=
  Id.run do
    let mut cnt := 0
    let mut queue : List Nat := [start]
    let mut vis : Std.HashSet Nat := {}
    while !queue.isEmpty do
      let x := queue.head!
      queue := queue.tail!
      if !vis.contains x then
        vis := vis.insert x
        cnt := cnt + 1
        for nb in neighbors x n do
          if grid[nb]! == area && !vis.contains nb then
            queue := nb :: queue
    return cnt

def checkArea (n : Nat) (grid : Array Nat) (area : Nat) : Bool :=
  let size := n * n
  let (start, total) :=
    Id.run do
      let mut st : Option Nat := none
      let mut tot := 0
      for idx in [0:size] do
        if grid[idx]! == area then
          tot := tot + 1
          if st.isNone then
            st := some idx
      return (st, tot)
  match start with
  | none => false
  | some s =>
      let cnt := bfsCount n grid area s
      cnt == total

def solveCase (n : Nat) (lines : Array String) : String :=
  let grid := Id.run do
    let size := n * n
    let mut g := Array.replicate size 0
    for i in [0:n-1] do
      if i < n - 1 then
        let tokens := (lines[i]!).trim.split (· = ' ') |>.filter (· ≠ "")
        let nums := tokens.toArray.map (·.toNat!)
        let len := nums.size / 2
        for j in [0:len] do
          let r := nums[2*j]! - 1
          let c := nums[2*j + 1]! - 1
          let idx := r * n + c
          g := g.set! idx (i + 1)
    for idx in [0:size] do
      if g[idx]! == 0 then
        g := g.set! idx n
    return g
  let ok := Id.run do
    let mut good := true
    for area in [1:n+1] do
      if !checkArea n grid area then
        good := false
    return good
  if ok then "good" else "wrong"

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let mut lines : Array String := Array.replicate (n - 1) ""
    for i in [0:n-1] do
      let l ← h.getLine
      lines := lines.set! i l
    IO.println (solveCase n lines)
    loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
