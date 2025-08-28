/- Solution for SPOJ DIV2 - Divisors 2
https://www.spoj.com/problems/DIV2/
-/

import Std
open Std

def limit : Nat := 1000000

/-- precompute divisor counts up to `limit` --/
def divisorCounts : Array Nat := Id.run do
  let mut d : Array Nat := Array.replicate (limit + 1) 0
  let mut i : Nat := 1
  while _h : i <= limit do
    let mut j : Nat := i
    while _h2 : j <= limit do
      d := d.set! j (d[j]! + 1)
      j := j + i
    i := i + 1
  d

/-- check if `n` satisfies the required property --/
def good (d : Array Nat) (n : Nat) : Bool :=
  let dn := d[n]!
  if dn <= 3 then
    false
  else
    Id.run do
      let mut k : Nat := 1
      let mut ok := true
      while _h : k * k <= n ∧ ok do
        if n % k = 0 then
          if dn % d[k]! ≠ 0 then
            ok := false
          else
            let m := n / k
            if m ≠ k && dn % d[m]! ≠ 0 then
              ok := false
        k := k + 1
      ok

def main : IO Unit := do
  let d := divisorCounts
  let mut cnt : Nat := 0
  let mut n : Nat := 1
  while _h : n <= limit do
    if good d n then
      if cnt % 108 == 107 then
        IO.println n
      cnt := cnt + 1
    n := n + 1
