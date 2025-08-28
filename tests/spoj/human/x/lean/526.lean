/- Solution for SPOJ DIV - Divisors
https://www.spoj.com/problems/DIV/
-/
import Std
open Std

/-- largest N considered --/
def maxN : Nat := 1000000

/-- check whether `d` is a product of two distinct primes --/
partial def isSemiprime (d : Nat) : Bool :=
  let rec loop (x p a b : Nat) : Bool :=
    if p * p > x then
      if x > 1 then
        if a = 0 then false
        else if b = 0 then a ≠ x
        else false
      else a ≠ 0 ∧ b ≠ 0 ∧ a ≠ b
    else if x % p = 0 then
      if (x / p) % p = 0 then
        false
      else if a = 0 then loop (x / p) (p + 1) p b
      else if b = 0 then loop (x / p) (p + 1) a p
      else false
    else
      loop x (p + 1) a b
  loop d 2 0 0

def main : IO Unit := do
  -- compute number of divisors for each n ≤ maxN
  let counts := Id.run do
    let mut cnts : Array Nat := Array.mkArray (maxN + 1) 0
    for i in [1:maxN+1] do
      let mut j := i
      while j ≤ maxN do
        cnts := cnts.set! j (cnts.get! j + 1)
        j := j + i
    return cnts
  -- collect all numbers whose divisor count is semiprime
  let mut arr : Array Nat := #[]
  for n in [1:maxN+1] do
    if isSemiprime (counts.get! n) then
      arr := arr.push n
  -- output every 9th element
  let mut i := 8
  while i < arr.size do
    IO.println (arr.get! i)
    i := i + 9
