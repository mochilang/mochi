/- Solution for SPOJ KPEQU - Equation
https://www.spoj.com/problems/KPEQU/
-/

import Std
open Std

/-- Sieve of Eratosthenes generating primes up to `n`. --/
def sieve (n : Nat) : Array Nat := Id.run do
  let mut isPrime := Array.replicate (n+1) true
  isPrime := isPrime.set! 0 false
  if n ≥ 1 then
    isPrime := isPrime.set! 1 false
  let mut res : Array Nat := #[]
  for i in [2:n+1] do
    if isPrime[i]! then
      res := res.push i
      let mut j := i * i
      while j ≤ n do
        isPrime := isPrime.set! j false
        j := j + i
  return res

/-- Exponent of prime `p` in `n!` (Legendre's formula). --/
def factExp (n p : Nat) : Nat :=
  let rec f (m acc : Nat) :=
    let q := m / p
    if q == 0 then acc else f q (acc + q)
  f n 0

/-- Number of solutions to `1/n! = 1/x + 1/y`. --/
def solve (n : Nat) (primes : Array Nat) : Nat :=
  let mut ans : Nat := 1
  for p in primes do
    if p ≤ n then
      let e := factExp n p
      ans := ans * (2 * e + 1)
  return ans

def main : IO Unit := do
  let primes := sieve 10000
  let data ← IO.FS.Stream.readToEnd (← IO.getStdin)
  let parts := data.split (fun c => c = ' ' || c = '\n' || c = '\r' || c = '\t')
  let mut nums : Array Nat := #[]
  for s in parts do
    if s.length > 0 then
      nums := nums.push s.toNat!
  for n in nums do
    if n == 0 then
      return
    else
      IO.println (solve n primes)
