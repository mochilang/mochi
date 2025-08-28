/- Solution for SPOJ MATRIX2 - Submatrix of submatrix
https://www.spoj.com/problems/MATRIX2/
-/

import Std
open Std

partial def slidingMin (arr : Array Int) (w : Nat) : Array Int := Id.run do
  let n := arr.size
  if w = 0 || w > n then
    return #[]
  let mut res : Array Int := Array.mkEmpty (n - w + 1)
  let mut dq : Array Nat := Array.mkArray n 0
  let mut front : Nat := 0
  let mut back : Nat := 0
  for i in [0:n] do
    while front < back && dq[front]! ≤ i - w do
      front := front + 1
    while front < back && arr[dq[back-1]!]! ≥ arr[i]! do
      back := back - 1
    dq := dq.set! back i
    back := back + 1
    if i + 1 ≥ w then
      res := res.push (arr[dq[front]!]!)
  return res

partial def rectSum (pref : Array (Array Int)) (i1 j1 i2 j2 : Nat) : Int :=
  (pref[i2]!.get! j2) - (pref[i1]!.get! j2) - (pref[i2]!.get! j1) + (pref[i1]!.get! j1)

partial def solve (n m A B C D : Nat) (first : Array Int) : Int := Id.run do
  -- build matrix
  let mut mat : Array (Array Int) := Array.mkArray n (Array.mkArray m 0)
  for i in [0:n] do
    let mut row : Array Int := Array.mkArray m 0
    let mut v := first[i]!
    row := row.set! 0 v
    for j in [1:m] do
      v := (v * 71 + 17) % 100 + 1
      row := row.set! j v
    mat := mat.set! i row
  -- prefix sums
  let mut pref : Array (Array Int) := Array.replicate (n+1) (Array.replicate (m+1) 0)
  for i in [0:n] do
    let top := pref[i]!
    let mut rowPref := pref[i+1]!
    let mut rowSum : Int := 0
    let row := mat[i]!
    for j in [0:m] do
      rowSum := rowSum + row[j]!
      let val := rowSum + top.get! (j+1)
      rowPref := rowPref.set! (j+1) val
    pref := pref.set! (i+1) rowPref
  -- inner minima
  let mut innerMin : Array (Array Int) := #[]
  if C > 0 && D > 0 then
    let rows1 := n - C + 1
    let cols1 := m - D + 1
    let mut s1 : Array (Array Int) := Array.mkArray rows1 (Array.mkArray cols1 0)
    for i in [0:rows1] do
      let mut r : Array Int := Array.mkArray cols1 0
      for j in [0:cols1] do
        let sum := rectSum pref i j (i + C) (j + D)
        r := r.set! j sum
      s1 := s1.set! i r
    let w := B - D - 1
    let h := A - C - 1
    let mut rowMins : Array (Array Int) := Array.mkArray rows1 (Array.mkEmpty 0)
    for i in [0:rows1] do
      rowMins := rowMins.set! i (slidingMin (s1[i]!) w)
    let cols2 := (rowMins[0]!).size
    let mut colMins : Array (Array Int) := Array.mkArray (rows1 - h + 1) (Array.mkArray cols2 0)
    for j in [0:cols2] do
      let mut colArr : Array Int := Array.mkEmpty rows1
      for i in [0:rows1] do
        colArr := colArr.push ((rowMins[i]!).get! j)
      let colRes := slidingMin colArr h
      for i in [0:rows1 - h + 1] do
        let row := colMins[i]!
        colMins := colMins.set! i (row.set! j (colRes.get! i))
    innerMin := colMins
  -- compute answer
  let mut best : Int := -1000000000
  for i in [0:n - A + 1] do
    for j in [0:m - B + 1] do
      let sumQ := rectSum pref i j (i + A) (j + B)
      let minK := if C = 0 || D = 0 then 0 else (innerMin[i+1]!).get! (j+1)
      let diff := sumQ - minK
      if diff > best then
        best := diff
  return best

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let tokensList := (data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun s => s ≠ "")
  let tokens := tokensList.toArray
  if tokens.size = 0 then return
  let mut idx := 0
  let t := (tokens[idx]!).toNat!; idx := idx + 1
  for _ in [0:t] do
    let n := (tokens[idx]!).toNat!; idx := idx + 1
    let m := (tokens[idx]!).toNat!; idx := idx + 1
    let a := (tokens[idx]!).toNat!; idx := idx + 1
    let b := (tokens[idx]!).toNat!; idx := idx + 1
    let c := (tokens[idx]!).toNat!; idx := idx + 1
    let d := (tokens[idx]!).toNat!; idx := idx + 1
    let mut first : Array Int := Array.mkEmpty n
    for _ in [0:n] do
      let v := (tokens[idx]!).toNat!; idx := idx + 1
      first := first.push v
    let ans := solve n m a b c d first
    IO.println ans
