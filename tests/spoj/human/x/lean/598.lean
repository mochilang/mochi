/- Solution for SPOJ INCR - Increasing Subsequences
https://www.spoj.com/problems/INCR/
-/

import Std
open Std

-- generate all partitions of n with parts not exceeding maxPart
partial def partitionsAux : Nat → Nat → List (List Nat)
| 0, _ => [[]]
| n+1, maxPart =>
  let upper := min (n+1) maxPart
  let rec loop : Nat → List (List Nat)
  | 0 => []
  | i+1 =>
    let rest := partitionsAux (n - i) (i+1)
    let cur := rest.map (fun p => (i+1) :: p)
    cur ++ loop i
  loop upper

def partitionsWithFirst (n k : Nat) : List (List Nat) :=
  if h : n < k then []
  else
    (partitionsAux (n - k) k).map (fun p => k :: p)

-- compute product of hook lengths for a given partition
partial def hookProduct (rows : List Nat) : Nat :=
  let rec rowLoop (rs : List Nat) (prod : Nat) : Nat :=
    match rs with
    | [] => prod
    | r :: tail =>
      let rec colLoop (j : Nat) (prod : Nat) : Nat :=
        if j > r then prod
        else
          let below := tail.foldl (fun c row => if row ≥ j then c + 1 else c) 0
          let hook := (r - j) + below + 1
          colLoop (j + 1) (prod * hook)
      rowLoop tail (colLoop 1 prod)
  rowLoop rows 1

-- count permutations of size n with LIS exactly k
partial def count (n k : Nat) : Nat :=
  if k = 0 ∨ k > n then 0
  else
    let fact := Nat.factorial n
    let parts := partitionsWithFirst n k
    parts.foldl (fun acc p =>
      let hook := hookProduct p
      let f := fact / hook
      let term := (f * f) % 1000000000
      (acc + term) % 1000000000
    ) 0

partial def parseAll (s : String) : List Nat :=
  s.split (fun c => c = ' ' ∨ c = '\n').filterMap String.toNat?

partial def process (data : List Nat) : List Nat :=
  let rec loop : List Nat → Nat → List Nat
  | [], _ => []
  | n::b::rest, t =>
      if t = 0 then []
      else (count n b) :: loop rest (t - 1)
  | _, _ => []
  match data with
  | [] => []
  | t::rest => loop rest t

def main : IO Unit := do
  let input ← IO.readStdin
  let nums := parseAll input
  let results := process nums
  for r in results do
    IO.println r
