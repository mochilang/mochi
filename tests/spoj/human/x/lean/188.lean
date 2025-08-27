/- Solution for SPOJ RECTNG1 - Rectangles
https://www.spoj.com/problems/RECTNG1/
-/

import Std
open Std

structure Rect where
  x1 y1 x2 y2 : Int

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Check if two rectangles share a common segment (edge or area). --/
def connected (a b : Rect) : Bool :=
  let xOverlap := min a.x2 b.x2 - max a.x1 b.x1
  let yOverlap := min a.y2 b.y2 - max a.y1 b.y1
  if xOverlap > 0 && yOverlap > 0 then true
  else if xOverlap > 0 && (a.y2 = b.y1 || b.y2 = a.y1) then true
  else if yOverlap > 0 && (a.x2 = b.x1 || b.x2 = a.x1) then true
  else false

/-- Disjoint-set find with path compression. --/
partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, p2) := find parent px
    (r, p2.set! x r)

/-- Union two sets. --/
def union (parent : Array Nat) (a b : Nat) : Array Nat :=
  let (ra, p1) := find parent a
  let (rb, p2) := find p1 b
  if ra = rb then p2 else p2.set! rb ra

/-- Solve one test case. --/
def solveCase (rects : Array Rect) : Nat :=
  let n := rects.size
  let mut parent : Array Nat := Array.mkArray n 0
  for i in [0:n] do
    parent := parent.set! i i
  for i in [0:n] do
    for j in [i+1:n] do
      if connected rects[i]! rects[j]! then
        parent := union parent i j
  let mut parent2 := parent
  let mut seen : Std.HashSet Nat := {}
  for i in [0:n] do
    let (r, p') := find parent2 i
    parent2 := p'
    seen := seen.insert r
  seen.size

/-- Process all test cases from the flattened integer array. --/
def process (data : Array Int) : String :=
  let t := (data.get! 0).toNat!
  let mut idx := 1
  let mut out := ""
  for case in [0:t] do
    let n := (data.get! idx).toNat!
    idx := idx + 1
    let mut rects : Array Rect := Array.mkArray n {x1 := 0, y1 := 0, x2 := 0, y2 := 0}
    for i in [0:n] do
      let x1 := data.get! idx; idx := idx + 1
      let y1 := data.get! idx; idx := idx + 1
      let x2 := data.get! idx; idx := idx + 1
      let y2 := data.get! idx; idx := idx + 1
      rects := rects.set! i { x1 := x1, y1 := y1, x2 := x2, y2 := y2 }
    let res := solveCase rects
    out := out ++ toString res
    if case + 1 < t then out := out ++ "\n"
  out

/-- Main program: read input and print results. --/
def main : IO Unit := do
  let data ← readInts
  IO.println (process data)
