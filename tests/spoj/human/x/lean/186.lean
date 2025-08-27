/- Solution for SPOJ LITELANG - The lightest language
https://www.spoj.com/problems/LITELANG/
-/
import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Remove and return the smallest element from the list. --/
def extractMin (xs : List Nat) : Nat × List Nat :=
  match xs with
  | [] => (0, [])
  | x :: xs =>
      let rec aux (m : Nat) (acc : List Nat) : List Nat → Nat × List Nat
        | []      => (m, acc.reverse)
        | y :: ys =>
            if y < m then aux y (m :: acc) ys else aux m (y :: acc) ys
      aux x [] xs

/-- Solve a single test case starting at index `start` in the data array.
    Returns the output string and the next index. -/
def solveCase (data : Array Nat) (start : Nat) : (String × Nat) := Id.run do
  let n := data.get! start
  let k := data.get! (start + 1)
  let mut idx := start + 2
  -- read weights
  let mut weights : List Nat := []
  for _ in [0:k] do
    weights := weights.concat (data.get! idx)
    idx := idx + 1
  let m := (n - 1 + (k - 2)) / (k - 1)
  let mut leaves : List Nat := [0]
  -- expand smallest leaves
  for _ in [0:m] do
    let (v, rest) := extractMin leaves
    leaves := rest ++ weights.map (fun w => v + w)
  -- sort leaves and sum n smallest
  let sorted := leaves.qsort (· < ·)
  let sum := (sorted.take n).foldl (· + ·) 0
  return (toString sum, idx)

/-- Main program: parse input and solve test cases. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (res, idx') := solveCase data idx
    IO.println res
    idx := idx'
