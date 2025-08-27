/- Solution for SPOJ PHONY - Phony Primes
https://www.spoj.com/problems/PHONY/
-/

import Std
open Std

/-- Sieve of Eratosthenes generating all primes ≤ `limit`. -/
private def primesUpTo (limit : Nat) : Array Nat :=
  Id.run do
    let mut isPrime := Array.mkArray (limit + 1) true
    if limit >= 0 then
      isPrime := isPrime.set! 0 false
    if limit >= 1 then
      isPrime := isPrime.set! 1 false
    let mut p := 2
    while p * p <= limit do
      if isPrime.get! p then
        let mut j := p * p
        while j <= limit do
          isPrime := isPrime.set! j false
          j := j + p
      p := p + 1
    let mut res := Array.empty
    let mut i := 2
    while i <= limit do
      if isPrime.get! i then
        res := res.push i
      i := i + 1
    return res

/-- Test if `n` is a phony prime (Carmichael number) using a list of primes. -/
private def isPhony (n : Nat) (primes : Array Nat) : Bool :=
  if n % 2 == 0 then
    false
  else
    Id.run do
      let mut m := n
      let mut cnt := 0
      let mut ok := true
      let mut idx := 0
      while ok && idx < primes.size && primes[idx]! * primes[idx]! <= m do
        let p := primes[idx]!
        if m % p == 0 then
          if (n - 1) % (p - 1) ≠ 0 then
            ok := false
          else
            cnt := cnt + 1
            m := m / p
            if m % p == 0 then
              ok := false
        idx := idx + 1
      if ok && m > 1 then
        if (n - 1) % (m - 1) ≠ 0 then
          ok := false
        else
          cnt := cnt + 1
      return ok && cnt ≥ 3

/-- Process a single test case printing all phony primes in `[a,b]`. -/
private def handle (a b : Nat) (primes : Array Nat) : IO Unit := do
  let mut found := false
  for n in [a:b.succ] do
    if isPhony n primes then
      IO.println n
      found := true
  if !found then
    IO.println "none"

/-- Main entry: read ranges until "0 0". -/
def main : IO Unit := do
  let primes := primesUpTo 46340
  let h ← IO.getStdin
  let rec loop : IO Unit := do
    let line := (← h.getLine).trim
    let parts := line.split (· = ' ') |>.filter (fun s => s ≠ "") |>.map (·.toNat!)
    let a := parts.get! 0
    let b := parts.get! 1
    if a == 0 && b == 0 then
      pure ()
    else
      handle a b primes
      loop
  loop
