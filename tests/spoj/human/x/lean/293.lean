/- Solution for SPOJ OFBEAT - Officers on the Beat
https://www.spoj.com/problems/OFBEAT/
-/

import Std
open Std

/-- Read all integers (possibly signed) from stdin. --/
def readInts : IO (Array Int) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  pure arr

/-- Remove the first occurrence of `x` from a list. --/
def removeOne (x : Int) : List Int → List Int
  | [] => []
  | y :: ys => if x = y then ys else y :: removeOne x ys

structure Seg where
  l : Int
  r : Int
  deriving Inhabited

/-- Collect vertical street segments as intervals on the Y axis. --/
def collectVertical (pts : Array (Int × Int)) : Array Seg := Id.run do
  let mut add : Std.HashMap Int (List Int) := Std.HashMap.empty
  let mut rem : Std.HashMap Int (List Int) := Std.HashMap.empty
  let mut coords : Std.HashSet Int := Std.HashSet.empty
  for i in [0:pts.size-1] do
    let (x1, y1) := pts[i]!
    let (x2, y2) := pts[i+1]!
    if y1 == y2 then
      let l := if x1 ≤ x2 then x1 else x2
      let r := if x1 ≤ x2 then x2 else x1
      if l ≠ r then
        add := add.insert l (y1 :: (add.findD l []))
        rem := rem.insert r (y1 :: (rem.findD r []))
        coords := coords.insert l
        coords := coords.insert r
  if coords.isEmpty then
    return #[]
  let mut xs := (coords.toList).qsort (fun a b => a < b)
  let last := xs.getLast! + 1
  xs := xs ++ [last]
  let xsArr := xs.toArray
  let mut active : List Int := []
  let mut segs : Array Seg := #[]
  for i in [0:xsArr.size-1] do
    let x := xsArr[i]!
    for y in rem.findD x [] do
      active := removeOne y active
    for y in add.findD x [] do
      active := y :: active
    let nxt := xsArr[i+1]!
    if nxt > x then
      let arr := (active.toArray).qsort (fun a b => a < b)
      let half := arr.size / 2
      for j in [0:half] do
        let y1 := arr.get! (2*j)
        let y2 := arr.get! (2*j + 1)
        segs := segs.push ⟨y1, y2⟩
  return segs

/-- Collect horizontal street segments as intervals on the X axis. --/
def collectHorizontal (pts : Array (Int × Int)) : Array Seg := Id.run do
  let mut add : Std.HashMap Int (List Int) := Std.HashMap.empty
  let mut rem : Std.HashMap Int (List Int) := Std.HashMap.empty
  let mut coords : Std.HashSet Int := Std.HashSet.empty
  for i in [0:pts.size-1] do
    let (x1, y1) := pts[i]!
    let (x2, y2) := pts[i+1]!
    if x1 == x2 then
      let l := if y1 ≤ y2 then y1 else y2
      let r := if y1 ≤ y2 then y2 else y1
      if l ≠ r then
        add := add.insert l (x1 :: (add.findD l []))
        rem := rem.insert r (x1 :: (rem.findD r []))
        coords := coords.insert l
        coords := coords.insert r
  if coords.isEmpty then
    return #[]
  let mut ys := (coords.toList).qsort (fun a b => a < b)
  let last := ys.getLast! + 1
  ys := ys ++ [last]
  let ysArr := ys.toArray
  let mut active : List Int := []
  let mut segs : Array Seg := #[]
  for i in [0:ysArr.size-1] do
    let y := ysArr[i]!
    for x in rem.findD y [] do
      active := removeOne x active
    for x in add.findD y [] do
      active := x :: active
    let nxt := ysArr[i+1]!
    if nxt > y then
      let arr := (active.toArray).qsort (fun a b => a < b)
      let half := arr.size / 2
      for j in [0:half] do
        let x1 := arr.get! (2*j)
        let x2 := arr.get! (2*j + 1)
        segs := segs.push ⟨x1, x2⟩
  return segs

/-- Greedy stabbing of intervals. --/
def cover (segs : Array Seg) : Nat :=
  let arr := segs.qsort (fun a b => a.r < b.r)
  let mut cnt : Nat := 0
  let mut last : Int := -1000000000000000000
  for s in arr do
    if last < s.l then
      cnt := cnt + 1
      last := s.r
  cnt

/-- Solve one test case given the edge lengths. --/
def solveCase (edges : Array Int) : Nat :=
  -- build polygon vertices
  let pts := Id.run do
    let mut pts : Array (Int × Int) := Array.mkEmpty (edges.size + 1)
    let mut x : Int := 0
    let mut y : Int := 0
    pts := pts.push (x, y)
    let mut horiz := true
    for a in edges do
      if horiz then x := x + a else y := y + a
      pts := pts.push (x, y)
      horiz := !horiz
    pts
  let vert := collectVertical pts
  let horz := collectHorizontal pts
  cover vert + cover horz

/-- Entry point. --/
def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then return
  let t := data[0]!.toNat
  let mut idx : Nat := 1
  for _ in [0:t] do
    let n := data[idx]!.toNat
    idx := idx + 1
    let mut edges : Array Int := Array.mkEmpty n
    for j in [0:n] do
      edges := edges.push data[idx + j]!
    idx := idx + n
    let ans := solveCase edges
    IO.println ans
