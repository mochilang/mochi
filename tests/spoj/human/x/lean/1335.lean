/- Solution for SPOJ KPMAZE - Maze
https://www.spoj.com/problems/KPMAZE/
-/

import Std
open Std

/-- compute determinant of integer matrix using Gaussian elimination on rationals -/
def det (mat : Array (Array Int)) : Int :=
  let n := mat.size
  if n == 0 then
    1
  else
    Id.run do
      let mut a : Array (Array Rat) := mat.map (fun row => row.map (fun v => Rat.ofInt v))
      let mut ans : Rat := 1
      for i in [0:n] do
        let mut pivot := i
        while pivot < n && a[pivot]![i] == 0 do
          pivot := pivot + 1
        if pivot == n then
          return 0
        if pivot != i then
          let rowi := a[i]!
          a := a.set! i a[pivot]!
          a := a.set! pivot rowi
          ans := -ans
        let piv := a[i]![i]
        ans := ans * piv
        for j in [i+1:n] do
          let factor := a[j]![i] / piv
          if factor != 0 then
            let mut rowj := a[j]!
            for k in [i:n] do
              rowj := rowj.set! k (rowj[k]! - factor * a[i]![k])
            a := a.set! j rowj
      return ans.num / (Int.ofNat ans.den)

/-- count spanning trees using Kirchhoff's theorem -/
def spanningTrees (n : Nat) (edges : List (Nat × Nat)) : Int :=
  if n <= 1 then
    1
  else
    let mut mat : Array (Array Int) := Array.replicate n (Array.replicate n 0)
    for (u, v) in edges do
      let i := u - 1
      let j := v - 1
      mat := mat.set! i (mat[i]!.set! i (mat[i]![i] + 1))
      mat := mat.set! j (mat[j]!.set! j (mat[j]![j] + 1))
      mat := mat.set! i (mat[i]!.set! j (mat[i]![j] - 1))
      mat := mat.set! j (mat[j]!.set! i (mat[j]![i] - 1))
    let size := n - 1
    let mut minor : Array (Array Int) := Array.mkEmpty size
    for i in [0:size] do
      let mut row : Array Int := Array.mkEmpty size
      for j in [0:size] do
        row := row.push mat[i]![j]
      minor := minor.push row
    det minor

/-- read a pair of natural numbers from the stream, skipping blank lines -/
partial def readNatPair (h : IO.FS.Stream) : IO (Nat × Nat) := do
  let line ← h.getLine
  let s := line.trim
  if s == "" then
    readNatPair h
  else
    let parts := s.split (· = ' ') |>.filter (· ≠ "")
    return (parts[0]!.toNat!, parts[1]!.toNat!)

/-- read four natural numbers from the stream, skipping blank lines -/
partial def readNatQuad (h : IO.FS.Stream) : IO (Nat × Nat × Nat × Nat) := do
  let line ← h.getLine
  let s := line.trim
  if s == "" then
    readNatQuad h
  else
    let parts := s.split (· = ' ') |>.filter (· ≠ "")
    return (parts[0]!.toNat!, parts[1]!.toNat!, parts[2]!.toNat!, parts[3]!.toNat!)

/-- read a single natural number from the stream, skipping blank lines -/
partial def readNat (h : IO.FS.Stream) : IO Nat := do
  let line ← h.getLine
  let s := line.trim
  if s == "" then
    readNat h
  else
    return s.toNat!

/-- check whether an edge is blocked by the given walls -/
def isBlocked (blocked : List (Nat × Nat)) (a b : Nat) : Bool :=
  let key := if a ≤ b then (a, b) else (b, a)
  blocked.contains key

/-- build edge list for a W×H grid with given blocked edges -/
def buildEdges (w h : Nat) (blocked : List (Nat × Nat)) : List (Nat × Nat) :=
  Id.run do
    let mut edges : List (Nat × Nat) := []
    for r in [0:h] do
      for c in [0:w] do
        let v := r * w + c + 1
        if r + 1 < h then
          let u := (r + 1) * w + c + 1
          if !isBlocked blocked v u then
            edges := (v, u) :: edges
        if c + 1 < w then
          let u := r * w + (c + 1) + 1
          if !isBlocked blocked v u then
            edges := (v, u) :: edges
    edges

/-- main: parse input, build graph, and output number of spanning trees -/
def main : IO Unit := do
  let h ← IO.getStdin
  let (w, hgt) ← readNatPair h
  let k ← readNat h
  let mut blocked : List (Nat × Nat) := []
  for _ in [0:k] do
    let (r1, c1, r2, c2) ← readNatQuad h
    let v1 := (r1 - 1) * w + c1
    let v2 := (r2 - 1) * w + c2
    let e := if v1 ≤ v2 then (v1, v2) else (v2, v1)
    blocked := e :: blocked
  let edges := buildEdges w hgt blocked
  let n := w * hgt
  IO.println (spanningTrees n edges)
