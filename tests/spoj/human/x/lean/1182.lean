/- Solution for SPOJ SORTBIT - Sorted bit sequence
https://www.spoj.com/problems/SORTBIT/
-/

import Std
open Std

-- compute binomial coefficient C(n,k)
def choose (n k : Nat) : Nat :=
  if k > n then 0
  else
    let k := if k > n - k then n - k else k
    let mut res := 1
    for i in [1:k+1] do
      res := res * (n - k + i) / i
    res

-- count numbers <= x with popcount = p
partial def countUpto (x p : Nat) : Nat :=
  let rec loop (i : Int) (p : Int) (res : Nat) : Nat :=
    if p < 0 then res
    else if i < 0 then
      if p = 0 then res + 1 else res
    else
      let idx := Int.toNat i
      if Nat.testBit x idx then
        let res := res + choose idx (Int.toNat p)
        loop (i - 1) (p - 1) res
      else
        loop (i - 1) p res
  loop 31 (Int.ofNat p) 0

-- count numbers in [l,r] with popcount = p
def countRange (l r p : Nat) : Nat :=
  if l = 0 then countUpto r p else countUpto r p - countUpto (l - 1) p

-- find popcount group and adjusted k
def findGroup (l r k : Nat) : Nat × Nat :=
  let rec go (p k : Nat) : Nat × Nat :=
    let c := countRange l r p
    if k > c then go (p + 1) (k - c) else (p, k)
  go 0 k

-- find k-th number in [l,r] with popcount p
def kthWithPop (l r p k : Nat) : Nat :=
  let rec bs (lo hi : Nat) (k : Nat) : Nat :=
    if lo = hi then lo
    else
      let mid := (lo + hi) / 2
      let c := countRange l mid p
      if c >= k then bs lo mid k else bs (mid + 1) hi (k - c)
  bs l r k

-- solve single test case
def solveCase (m n : Int) (k : Nat) : Int :=
  let base : Int := 4294967296
  let half : Int := 2147483648
  let toUnsigned (x : Int) : Nat :=
    if x >= 0 then Int.toNat x else Int.toNat (x + base)
  let l := toUnsigned m
  let r := toUnsigned n
  let (p, k') := findGroup l r k
  let ansU := kthWithPop l r p k'
  let ans := Int.ofNat ansU
  if ans >= half then ans - base else ans

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let parts := line.split (· = ' ') |>.filter (· ≠ "")
      let m := parts.get! 0 |>.toInt!
      let n := parts.get! 1 |>.toInt!
      let k := parts.get! 2 |>.toNat!
      IO.println (solveCase m n k)
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
