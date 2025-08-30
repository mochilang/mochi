/- Solution for SPOJ WIJGT - Will Indiana Jones Get There
https://www.spoj.com/problems/WIJGT/
-/

import Std
open Std

structure Seg where
  x1 : Int
  y1 : Int
  x2 : Int
  y2 : Int

structure Edge where
  w : Float
  a : Nat
  b : Nat

/-- compute Euclidean distance between two axis-aligned segments --/
def dist (s t : Seg) : Float :=
  let ax1 := s.x1.toFloat
  let ay1 := s.y1.toFloat
  let ax2 := s.x2.toFloat
  let ay2 := s.y2.toFloat
  let bx1 := t.x1.toFloat
  let by1 := t.y1.toFloat
  let bx2 := t.x2.toFloat
  let by2 := t.y2.toFloat
  let dx :=
    if ax2 < bx1 then bx1 - ax2
    else if bx2 < ax1 then ax1 - bx2
    else 0.0
  let dy :=
    if ay2 < by1 then by1 - ay2
    else if by2 < ay1 then ay1 - by2
    else 0.0
  Float.sqrt (dx*dx + dy*dy)

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
def solveCase (segs : Array Seg) : Float :=
  let n := segs.size
  let mut edges : Array Edge := #[]
  for i in [0:n] do
    for j in [i+1:n] do
      edges := edges.push {
        w := dist (segs[i]!) (segs[j]!),
        a := i,
        b := j
      }
  let edges := edges.qsort (fun e1 e2 => e1.w < e2.w)
  let mut parent : Array Nat := Array.mkArray n 0
  for i in [0:n] do
    parent := parent.set! i i
  let rec go (idx : Nat) (parent : Array Nat) : Float :=
    if idx ≥ edges.size then
      0.0
    else
      let e := edges[idx]!
      let parent := union parent e.a e.b
      let (r0, p1) := find parent 0
      let (r1, p2) := find p1 1
      if r0 = r1 then e.w else go (idx+1) p2
  go 0 parent

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Format float with two decimals. --/
def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

/-- Process all test cases from flattened integers. --/
partial def process (data : Array Int) (idx : Nat) (acc : List String) : List String :=
  let n := data.get! idx
  if n = 0 then
    acc.reverse
  else
    let nNat := n.toNat!
    let mut segs : Array Seg := #[]
    let mut j := idx + 1
    for _ in [0:nNat] do
      let x := data.get! j; let y := data.get! (j+1); let l := data.get! (j+2)
      j := j + 3
      if l ≥ 0 then
        segs := segs.push {
          x1 := x, y1 := y, x2 := x + l, y2 := y
        }
      else
        let len := -l
        segs := segs.push {
          x1 := x, y1 := y, x2 := x, y2 := y + len
        }
    let ans := solveCase segs
    process data j (format2 ans :: acc)

/-- Main entry point. --/
def main : IO Unit := do
  let data ← readInts
  let outs := process data 0 []
  IO.println (String.intercalate "\n" outs)
