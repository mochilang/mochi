/- Solution for SPOJ WINDOW1 - Window
https://www.spoj.com/problems/WINDOW1/
-/

import Std
open Std

structure Seg where
  l : Int
  r : Int

instance : Inhabited Seg := ⟨⟨0,0⟩⟩
/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let stdin ← IO.getStdin
  let s ← stdin.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

partial def dsuFind (parentRef : IO.Ref (Array Nat)) (x : Nat) : IO Nat := do
  let parent ← parentRef.get
  let p := parent[x]!
  if p == x then
    return x
  else
    let r ← dsuFind parentRef p
    let parent ← parentRef.get
    parentRef.set (parent.set! x r)
    return r

/-- Solve a single test case starting at index `start` in `data`.
    Returns output string and new index. -/
def solveCase (data : Array Int) (start : Nat) : IO (String × Nat) := do
  let x1 := data[start]!
  let y1 := data[start + 1]!
  let x2 := data[start + 2]!
  let y2 := data[start + 3]!
  let n := data[start + 4]!
  let nNat := Int.toNat n
  let mut idx := start + 5
  let mut verts : Array (Int × Int) := Array.mkEmpty (α := (Int × Int)) nNat
  for _ in [0:nNat] do
      let vx := data[idx]!
      let vy := data[idx + 1]!
      verts := verts.push (vx, vy)
      idx := idx + 2
  let rows := Int.toNat (y1 - y2)
  let mut segRows : Array (Array Seg) := Array.replicate rows #[]
  for r in [0:rows] do
    let y := y2 + Int.ofNat r
    let mut xs : Array Int := #[]
    for i in [0:nNat] do
      let v1 := verts[i]!
      let (ax, ay) := v1
      let idx2 := Nat.mod (i + 1) nNat
      let v2 := verts[idx2]!
      let (bx, by') := v2
      if ax == bx then
        let lo := min ay by'
        let hi := max ay by'
        if y >= lo && y < hi then
          xs := xs.push ax
      xs := xs.qsort (· < ·)
      let pairs := xs.size / 2
      let mut segs : Array Seg := #[]
      for j in [0:pairs] do
        let a := xs[2*j]!
        let b := xs[2*j + 1]!
        let l := max a x1
        let r := min b x2
        if l < r then
          segs := segs.push ⟨l, r⟩
      segRows := segRows.set! r segs
  let mut total := 0
  let mut idsRows : Array (Array Nat) := Array.replicate rows #[]
  for r in [0:rows] do
    let segs := segRows[r]!
    let mut ids : Array Nat := Array.mkEmpty segs.size
    for _ in segs do
      ids := ids.push total
      total := total + 1
    idsRows := idsRows.set! r ids
  let parentRef ← IO.mkRef (Array.range total)
  for r in [0:rows-1] do
    let segs1 := segRows[r]!
    let segs2 := segRows[r + 1]!
    let ids1 := idsRows[r]!
    let ids2 := idsRows[r + 1]!
    let mut i : Nat := 0
    let mut j : Nat := 0
      while i < segs1.size && j < segs2.size do
        let s1 := segs1[i]!
        let s2 := segs2[j]!
        if s1.r <= s2.l then
          i := i + 1
        else if s2.r <= s1.l then
          j := j + 1
        else
          let id1 := ids1[i]!
          let id2 := ids2[j]!
          let ra ← dsuFind parentRef id1
          let rb ← dsuFind parentRef id2
          if ra != rb then
            let parent ← parentRef.get
            parentRef.set (parent.set! ra rb)
          if s1.r < s2.r then
            i := i + 1
          else
            j := j + 1
  let mut set : Std.HashSet Nat := {}
  for r in [0:rows] do
    let ids := idsRows[r]!
    for id in ids do
      let root ← dsuFind parentRef id
      set := set.insert root
  let ans := set.size
  return (toString ans, idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then return
  let t := Int.toNat (data[0]!)
  let mut idx := 1
  for _ in [0:t] do
    let (res, idx') ← solveCase data idx
    IO.println res
    idx := idx'
