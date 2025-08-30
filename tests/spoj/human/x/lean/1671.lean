/- Solution for SPOJ AMATH - Another Mathematical Problem
https://www.spoj.com/problems/AMATH/
-/

import Std
open Std

/-- modular exponentiation --/
partial def powMod (a b m : Nat) : Nat :=
  if b == 0 then
    1 % m
  else
    let h := powMod a (b / 2) m
    let hh := (h * h) % m
    if b % 2 == 0 then hh else (hh * (a % m)) % m

/-- p-adic valuation --/
partial def vPow (p n : Nat) : Nat :=
  if n % p == 0 then 1 + vPow p (n / p) else 0

/-- multiplicative order of n modulo p^k (n and p coprime) --/
partial def order (n p k : Nat) : Nat :=
  let m := Nat.pow p k
  let phi := (p - 1) * Nat.pow p (k - 1)
  let rec step (ord : Nat) (primes : List Nat) : Nat :=
    match primes with
    | [] => ord
    | q :: qs =>
        let rec divide (o : Nat) : Nat :=
          if o % q == 0 && powMod n (o / q) m == 1 then divide (o / q) else o
        step (divide ord) qs
  step phi [2,5]

/-- read all natural numbers from stdin --/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ 48 && c ≤ 57 then
      num := num * 10 + (c.toNat - 48)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then arr := arr.push num
  pure arr

/-- solve one test case --/
def solveCase (n k : Nat) : Option Nat :=
  let v2 := vPow 2 n
  let v5 := vPow 5 n
  if (v2 > 0 && v2 < k) || (v5 > 0 && v5 < k) then
    none
  else
    let t2 := if v2 ≥ k then 1 else order n 2 k
    let t5 := if v5 ≥ k then 1 else order n 5 k
    some (Nat.lcm t2 t5)

/-- main: process all pairs until EOF --/
def main : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let mut outs : Array String := #[]
  while idx + 1 < nums.size do
    let n := nums[idx]!
    let k := nums[idx + 1]!
    idx := idx + 2
    match solveCase n k with
    | some t => outs := outs.push (toString t)
    | none => outs := outs.push "-1"
  IO.println (String.intercalate "\n" outs.toList)
