/- Solution for SPOJ KPMATRIX - Matrix
https://www.spoj.com/problems/KPMATRIX/
-/
import Std
open Std

/-- least significant set bit --/
@[inline] def lowbit (x : Nat) : Nat :=
  x - Nat.land x (x - 1)

/-- add `delta` at index `idx` in Fenwick tree `bit` of size `n` --/
def bitUpdate (bit : Array Int) (n idx : Nat) (delta : Int) : Array Int :=
  Id.run do
    let mut b := bit
    let mut i := idx
    while _h : i <= n do
      b := b.set! i (b[i]! + delta)
      let lb := lowbit i
      i := i + lb
    pure b

/-- prefix sum query for Fenwick tree --/
def bitQuery (bit : Array Int) (idx : Nat) : Int :=
  Id.run do
    let mut s : Int := 0
    let mut i := idx
    while _h : i > 0 do
      s := s + bit[i]!
      let lb := lowbit i
      i := i - lb
    pure s

/-- number of elements in sorted array `arr` less or equal to `x` --/
@[inline] def countLE (arr : Array Int) (x : Int) : Nat :=
  let rec go (l r : Nat) : Nat :=
    if h : l < r then
      let m := (l + r) / 2
      if arr.get! m <= x then
        go (m+1) r
      else
        go l m
    else l
  go 0 arr.size

/-- count subarrays with sum in [A,B] --/
def countSubarrays (arr : Array Int) (A B : Int) : Int :=
  let m := arr.size
  Id.run do
    -- prefix sums
    let mut ps : Array Int := Array.mkArray (m+1) 0
    let mut s : Int := 0
    let mut i : Nat := 0
    while _h : i < m do
      s := s + arr[i]!
      ps := ps.set! (i+1) s
      i := i + 1
    -- coordinate compression
    let mut sorted := ps
    sorted := sorted.qsort (fun a b => a < b)
    let mut uniq : Array Int := Array.mkEmpty sorted.size
    let mut j : Nat := 0
    let mut prev : Option Int := none
    while _h : j < sorted.size do
      let v := sorted[j]!
      match prev with
      | some p => if v != p then uniq := uniq.push v else pure ()
      | none   => uniq := uniq.push v
      prev := some v
      j := j + 1
    let size := uniq.size
    -- mapping value -> index
    let mut mp : Std.RBMap Int Nat compare := {}
    let mut k : Nat := 0
    while _h : k < size do
      mp := mp.insert (uniq[k]!) (k+1)
      k := k + 1
    -- Fenwick tree
    let mut bit : Array Int := Array.replicate (size+1) 0
    let idx0 := mp.find! 0
    bit := bitUpdate bit size idx0 1
    let mut ans : Int := 0
    let mut t : Nat := 1
    while _h : t ≤ m do
      let cur := ps[t]!
      let r := countLE uniq (cur - A)
      let l := countLE uniq (cur - B - 1)
      ans := ans + (bitQuery bit r - bitQuery bit l)
      let idx := mp.find! cur
      bit := bitUpdate bit size idx 1
      t := t + 1
    pure ans

/-- count submatrices with sum in [A,B] --/
def countSubmatrices (mat : Array (Array Int)) (n m : Nat) (A B : Int) : Int :=
  Id.run do
    let mut ans : Int := 0
    let mut top : Nat := 0
    while _hTop : top < n do
      let mut col : Array Int := Array.replicate m 0
      let mut bottom : Nat := top
      while _hBot : bottom < n do
        let row := mat[bottom]!
        let mut j : Nat := 0
        while _hJ : j < m do
          col := col.set! j (col[j]! + row[j]!)
          j := j + 1
        ans := ans + countSubarrays col A B
        bottom := bottom + 1
      top := top + 1
    pure ans

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "")
  let arr : Array String := List.toArray toks
  let nums := arr.map (fun s => s.toInt!)
  let mut idx : Nat := 0
  let n := nums[idx]!.toNat; idx := idx + 1
  let m := nums[idx]!.toNat; idx := idx + 1
  let mut mat : Array (Array Int) := Array.mkEmpty n
  let mut i : Nat := 0
  while _h : i < n do
    let mut row : Array Int := Array.mkEmpty m
    let mut j : Nat := 0
    while _h2 : j < m do
      row := row.push nums[idx]!
      idx := idx + 1
      j := j + 1
    mat := mat.push row
    i := i + 1
  let A := nums[idx]!; idx := idx + 1
  let B := nums[idx]!
  let res := countSubmatrices mat n m A B
  IO.println res
