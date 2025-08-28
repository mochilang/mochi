/- Solution for SPOJ SWAPS - Counting inversions
https://www.spoj.com/problems/SWAPS/
-/

import Std
open Std

/-- merge sort counting inversions -/
partial def mergeCount (a : Array Nat) : (Array Nat × Int) :=
  if h : a.size <= 1 then
    (a, 0)
  else
    let mid := a.size / 2
    let (left, invL) := mergeCount (a.extract 0 mid)
    let (right, invR) := mergeCount (a.extract mid a.size)
    Id.run do
      let mut i : Nat := 0
      let mut j : Nat := 0
      let mut merged : Array Nat := Array.mkEmpty a.size
      let mut inv := invL + invR
      while i < left.size && j < right.size do
        if left[i]! <= right[j]! then
          merged := merged.push left[i]!
          i := i + 1
        else
          merged := merged.push right[j]!
          inv := inv + Int.ofNat (left.size - i)
          j := j + 1
      while i < left.size do
        merged := merged.push left[i]!
        i := i + 1
      while j < right.size do
        merged := merged.push right[j]!
        j := j + 1
      return (merged, inv)

/-- simple integer square root -/
def natSqrt (n : Nat) : Nat :=
  Id.run do
    let mut x := 0
    while (x + 1) * (x + 1) ≤ n do
      x := x + 1
    return x

/-- lower bound on sorted array -/
def lowerBound (a : Array Nat) (x : Nat) : Nat :=
  Id.run do
    let mut l := 0
    let mut r := a.size
    while l < r do
      let m := (l + r) / 2
      if a[m]! < x then
        l := m + 1
      else
        r := m
    return l

/-- upper bound on sorted array -/
def upperBound (a : Array Nat) (x : Nat) : Nat :=
  Id.run do
    let mut l := 0
    let mut r := a.size
    while l < r do
      let m := (l + r) / 2
      if a[m]! <= x then
        l := m + 1
      else
        r := m
    return l

/-- count elements before position greater than val -/
def countGreaterBefore (arr : Array Nat) (blocks : Array (Array Nat))
    (blockSize pos val : Nat) : Nat :=
  Id.run do
    let mut res := 0
    let b := pos / blockSize
    for k in [0:b] do
      let blk := blocks[k]!
      res := res + blk.size - upperBound blk val
    let start := b * blockSize
    for i in [start:pos] do
      if arr[i]! > val then
        res := res + 1
    return res

/-- count elements after position less than val -/
def countLessAfter (arr : Array Nat) (blocks : Array (Array Nat))
    (n blockSize pos val : Nat) : Nat :=
  Id.run do
    let mut res := 0
    let b := pos / blockSize
    let endB := min n ((b + 1) * blockSize)
    for i in [pos+1:endB] do
      if arr[i]! < val then
        res := res + 1
    for k in [b+1:blocks.size] do
      let blk := blocks[k]!
      res := res + lowerBound blk val
    return res

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    return ()
  let mut idx := 0
  let n := toks[idx]!.toNat!
  idx := idx + 1
  let mut arr : Array Nat := Array.mkEmpty n
  for _ in [0:n] do
    arr := arr.push (toks[idx]!.toNat!)
    idx := idx + 1
  let (_, inv0) := mergeCount arr
  let mut inv : Int := inv0
  let blockSize := natSqrt n + 1
  let numBlocks := (n + blockSize - 1) / blockSize
  let mut blocks : Array (Array Nat) := Array.replicate numBlocks Array.empty
  for b in [0:numBlocks] do
    let start := b * blockSize
    let stop := min n (start + blockSize)
    let slice := arr.extract start stop
    blocks := blocks.set! b (slice.qsort (· < ·))
  let m := toks[idx]!.toNat!
  idx := idx + 1
  let mut outLines : Array String := Array.mkEmpty m
  for _ in [0:m] do
    let pos1 := toks[idx]!.toNat!
    let pos := pos1 - 1
    let newVal := toks[idx+1]!.toNat!
    idx := idx + 2
    let oldVal := arr[pos]!
    let beforeLess := countLessAfter arr blocks n blockSize pos oldVal
    let beforeGreater := countGreaterBefore arr blocks blockSize pos oldVal
    let afterLess := countLessAfter arr blocks n blockSize pos newVal
    let afterGreater := countGreaterBefore arr blocks blockSize pos newVal
    inv := inv - Int.ofNat beforeLess - Int.ofNat beforeGreater
    inv := inv + Int.ofNat afterLess + Int.ofNat afterGreater
    arr := arr.set! pos newVal
    let b := pos / blockSize
    let start := b * blockSize
    let stop := min n (start + blockSize)
    blocks := blocks.set! b ((arr.extract start stop).qsort (· < ·))
    outLines := outLines.push (toString inv)
  for line in outLines do
    IO.println line
