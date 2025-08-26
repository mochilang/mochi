/- Solution for SPOJ CUTOUT - Cutting out
https://www.spoj.com/problems/CUTOUT/
-/

import Std
open Std

private def parseInts (line : String) : List Nat :=
  line.trim.split (· = ' ') |>.filterMap (fun s =>
    if s.isEmpty then none else some s.toNat!)

private def buildRects : List Nat -> List (Nat × Nat × Nat × Nat)
  | a :: b :: c :: d :: rest => (a, b, c, d) :: buildRects rest
  | _ => []

private def dedupSorted (a : Array Nat) : Array Nat :=
  let sorted := a.qsort (· < ·)
  Id.run do
    let mut res : Array Nat := #[]
    for x in sorted do
      if res.isEmpty || res.back? ≠ some x then
        res := res.push x
    return res

private def diffs (arr : Array Nat) : Array Nat :=
  Id.run do
    let mut res : Array Nat := Array.mkArray (arr.size - 1) 0
    for i in [0:arr.size-1] do
      res := res.set! i (arr[i+1]! - arr[i]!)
    return res

private def findIdx (arr : Array Nat) (v : Nat) : Nat :=
  Id.run do
    for i in [0:arr.size] do
      if arr[i]! = v then
        return i
    return 0

private def solve (n : Nat) (rects : List (Nat × Nat × Nat × Nat)) : Nat :=
  Id.run do
    let mut xsArr : Array Nat := #[0, n]
    let mut ysArr : Array Nat := #[0, n]
    for (x1, x2, y1, y2) in rects do
      xsArr := xsArr.push x1; xsArr := xsArr.push x2
      ysArr := ysArr.push y1; ysArr := ysArr.push y2
    let xs := dedupSorted xsArr
    let ys := dedupSorted ysArr
    let cols := xs.size - 1
    let rows := ys.size - 1
    let w := diffs xs
    let h := diffs ys
    let mut blocked : Array (Array Bool) := Array.replicate rows (Array.replicate cols false)
    for (x1, x2, y1, y2) in rects do
      let xi1 := findIdx xs x1
      let xi2 := findIdx xs x2
      let yi1 := findIdx ys y1
      let yi2 := findIdx ys y2
      for j in [yi1:yi2] do
        let mut row := blocked[j]!
        for i in [xi1:xi2] do
          row := row.set! i true
        blocked := blocked.set! j row
    let mut heights := Array.replicate cols (0 : Nat)
    let mut best : Nat := 0
    for rIdx in [0:rows] do
      let row := blocked[rIdx]!
      for cIdx in [0:cols] do
        if row[cIdx]! then
          heights := heights.set! cIdx 0
        else
          heights := heights.set! cIdx (heights[cIdx]! + h[rIdx]!)
      for l in [0:cols] do
        let base := heights[l]!
        if base > 0 then
          let mut minH := base
          let mut width := w[l]!
          let mut area := minH * width
          if area > best then best := area
          for r in [l+1:cols] do
            minH := Nat.min minH (heights[r]!)
            width := width + w[r]!
            area := minH * width
            if area > best then best := area
    return best

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line1 ← h.getLine
    let ws := line1.trim.split (· = ' ')
    let n := ws.get! 0 |>.toNat!
    let r := ws.get! 1 |>.toNat!
    let line2 ← h.getLine
    let nums := parseInts line2
    let rects := buildRects nums
    let ans := solve n rects
    IO.println ans
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
