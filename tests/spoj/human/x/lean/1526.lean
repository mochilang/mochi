/- Solution for SPOJ RSORTING - Ranklist Sorting
https://www.spoj.com/problems/RSORTING/
-/

import Std
open Std

/-- find index of an element (0-based) -/
partial def findIndex (xs : List Nat) (x : Nat) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => if y = x then 0 else findIndex ys x + 1

/-- remove element at position `i` (0-based) -/
def removeAt (xs : List α) (i : Nat) : List α :=
  xs.take i ++ xs.drop (i + 1)

/-- insert `x` at position `i` (0-based) -/
def insertAt (xs : List α) (i : Nat) (x : α) : List α :=
  xs.take i ++ (x :: xs.drop i)

/-- compute indices of LIS as boolean array -/
def lisKeep (r : Array Nat) : Array Bool :=
  Id.run do
    let n := r.size
    let mut dp : Array Nat := Array.mkArray n 1
    let mut prev : Array Nat := Array.mkArray n 0
    let mut bestLen := 0
    let mut bestIdx := 0
    for i in [0:n] do
      for j in [0:i] do
        if r.get! j < r.get! i && dp.get! j + 1 > dp.get! i then
          dp := dp.set! i (dp.get! j + 1)
          prev := prev.set! i (j + 1)
      if dp.get! i > bestLen then
        bestLen := dp.get! i
        bestIdx := i
    let mut keep : Array Bool := Array.mkArray n false
    let mut cur := bestIdx
    while true do
      keep := keep.set! cur true
      let p := prev.get! cur
      if p = 0 then
        break
      cur := p - 1
    return keep

/-- apply moves to transform current rank list -/
partial def applyMoves : List Nat → List Nat → List (Nat × Nat)
| arr, [] => []
| arr, x :: xs =>
  let idx := findIndex arr x
  let arr1 := removeAt arr idx
  let arr2 := insertAt arr1 (x - 1) x
  (idx + 1, x) :: applyMoves arr2 xs

/-- solve a single test case -/
def solveCase (scores : List Nat) : List String :=
  let n := scores.length
  let sorted := scores.mergeSort (fun a b => decide (a > b))
  let rankMap : Std.HashMap Nat Nat := Id.run do
    let mut m : Std.HashMap Nat Nat := {}
    let mut idx := 1
    for v in sorted do
      m := m.insert v idx
      idx := idx + 1
    return m
  let ranks := scores.map (fun s => rankMap.find! s)
  let rArr := ranks.toArray
  let keepArr := lisKeep rArr
  let toMoveList := Id.run do
    let mut lst : List Nat := []
    for i in [0:n] do
      if !keepArr.get! i then
        lst := rArr.get! i :: lst
    return lst
  let toMove := toMoveList.mergeSort (fun a b => decide (a <= b))
  let moves := applyMoves ranks toMove
  (toString moves.length) :: moves.map (fun (i,j) => s!"{i} {j}")

/-- parse all numbers from input string -/
partial def parseAll (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filterMap String.toNat?

/-- process all test cases -/
partial def process : List Nat → List (List String)
| [] => []
| n :: rest =>
    let scores := rest.take n
    let remain := rest.drop n
    let out := solveCase scores
    out :: process remain

def main : IO Unit := do
  let data ← IO.readStdin
  let nums := parseAll data
  let outs := process nums
  for out in outs do
    for line in out do
      IO.println line
