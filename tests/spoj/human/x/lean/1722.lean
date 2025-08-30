/- Solution for SPOJ RP - Life, the Universe, and Everything II
https://www.spoj.com/problems/RP/
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

/-- generate graph edges for given k and n -/
def genEdges (k n : Nat) : List (Nat × Nat) :=
  Id.run do
    let mut edges : List (Nat × Nat) := []
    for i in [1:n+1] do
      let hi := Nat.min (n+1) (i + k + 1)
      for j in [i+1:hi] do
        edges := (i, j) :: edges
    return edges

/-- count spanning trees using Kirchhoff's theorem -/
def spanningTrees (n k : Nat) : Int :=
  if n <= 1 then
    1
  else
    let edges := genEdges k n
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

partial def readNatPair (h : IO.FS.Stream) : IO (Nat × Nat) := do
  let line ← h.getLine
  let s := line.trim
  if s == "" then
    readNatPair h
  else
    let parts := s.split (· = ' ') |>.filter (· ≠ "")
    return (parts[0]!.toNat!, parts[1]!.toNat!)

partial def readNat (h : IO.FS.Stream) : IO Nat := do
  let line ← h.getLine
  let s := line.trim
  if s == "" then
    readNat h
  else
    return s.toNat!

partial def solveCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let (k, n) ← readNatPair h
    let ans := spanningTrees n k
    IO.println (Int.toNat (ans % 65521))
    solveCases h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t ← readNat h
  solveCases h t
