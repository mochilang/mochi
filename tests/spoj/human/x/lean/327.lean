/- Solution for SPOJ PLATON - Platon and Socrates
https://www.spoj.com/problems/PLATON/
-/

import Std
open Std

/-- Read all natural numbers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- A pair together with its sum and product. --/
structure Pair where
  a b sum prod : Nat
  deriving Repr

/-- Return the pairs satisfying the dialogue for range [l, r]. --/
def solve (l r : Nat) : List (Nat × Nat) := Id.run do
  -- enumerate all pairs and count products
  let mut pairs : Array Pair := #[]
  let mut prodCount : Std.HashMap Nat Nat := {}
  for a in [l:r+1] do
    for b in [a+1:r+1] do
      let s := a + b
      let p := a * b
      pairs := pairs.push ⟨a, b, s, p⟩
      let c := match prodCount.get? p with
               | some v => v + 1
               | none   => 1
      prodCount := prodCount.insert p c
  -- map sums to their pairs
  let mut sumMap : Std.HashMap Nat (Array Pair) := {}
  for pr in pairs do
    let arr := match sumMap.get? pr.sum with
               | some a => a
               | none   => #[]
    sumMap := sumMap.insert pr.sum (arr.push pr)
  -- sums where every pair has ambiguous product and at least two pairs
  let mut validSums : Std.HashSet Nat := {}
  for (s, arr) in sumMap.toList do
    if h : arr.size ≥ 2 then
      let ok := arr.all (fun pr => match prodCount.get? pr.prod with
                                   | some c => c > 1
                                   | none   => false)
      if ok then
        validSums := validSums.insert s
  -- candidate pairs after Socrates' statement
  let cand2 := pairs.filter (fun pr =>
      (match prodCount.get? pr.prod with | some c => c > 1 | none => false) &&
      validSums.contains pr.sum)
  -- Platon now knows: product unique among cand2
  let mut prodCount2 : Std.HashMap Nat Nat := {}
  for pr in cand2 do
    let c := match prodCount2.get? pr.prod with
             | some v => v + 1
             | none   => 1
    prodCount2 := prodCount2.insert pr.prod c
  let cand3 := cand2.filter (fun pr => match prodCount2.get? pr.prod with
                                       | some 1 => true
                                       | _ => false)
  -- Socrates knows too: sum unique among cand3
  let mut sumCount2 : Std.HashMap Nat Nat := {}
  for pr in cand3 do
    let c := match sumCount2.get? pr.sum with
             | some v => v + 1
             | none   => 1
    sumCount2 := sumCount2.insert pr.sum c
  let final := cand3.filter (fun pr => match sumCount2.get? pr.sum with
                                       | some 1 => true
                                       | _ => false)
  -- sort by sum, then a, then b
  let arr := final.toList.qsort (fun p1 p2 =>
      if p1.sum == p2.sum then
        if p1.a == p2.a then p1.b < p2.b else p1.a < p2.a
      else
        p1.sum < p2.sum)
  return arr.map (fun pr => (pr.a, pr.b))

/-- Main program: read input, solve each test case, and print results. --/
def main : IO Unit := do
  let data ← readInts
  let t := data[0]!
  let mut idx := 1
  for case in [1:t+1] do
    let l := data[idx]!; let r := data[idx+1]!; idx := idx + 2
    let pairs := solve l r
    IO.println s!"case {case}"
    IO.println pairs.length
    for (a,b) in pairs do
      IO.println s!"{a} {b}"
