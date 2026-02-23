/- Solution for SPOJ PPATH - Prime Path
https://www.spoj.com/problems/PPATH/
-/

import Std
open Std

/-- Sieve of Eratosthenes generating a boolean array marking primes up to `n`. -/
def sieve (n : Nat) : Array Bool := Id.run do
  let mut isPrime := Array.replicate (n+1) true
  isPrime := isPrime.set! 0 false
  if n >= 1 then
    isPrime := isPrime.set! 1 false
  for i in [2:n+1] do
    if isPrime[i]! then
      let mut j := i * i
      while j <= n do
        isPrime := isPrime.set! j false
        j := j + i
  return isPrime

/-- Generate neighbouring primes by changing one digit at a time. -/
def neighbors (n : Nat) (isPrime : Array Bool) : Array Nat := Id.run do
  let mut res : Array Nat := #[]
  let pow : Array Nat := #[1,10,100,1000]
  for pos in [0:4] do
    let p := pow[pos]!
    let digit := (n / p) % 10
    let start := if pos = 3 then 1 else 0
    for d in [start:10] do
      if d ≠ digit then
        let cand := n - digit*p + d*p
        if isPrime[cand]! then
          res := res.push cand
  return res

/-- Breadth-first search from `start` to `target` over the prime graph. -/
def bfs (start target : Nat) (isPrime : Array Bool) : Option Nat := Id.run do
  let mut visited := Array.replicate 10000 false
  let mut q : Std.Queue (Nat × Nat) := .empty
  visited := visited.set! start true
  q := q.enqueue (start, 0)
  let mut ans : Option Nat := none
  while ans.isNone do
    match q.dequeue? with
    | none => break
    | some ((x,d), q') =>
        q := q'
        if x = target then
          ans := some d
        else
          for y in neighbors x isPrime do
            if !visited[y]! then
              visited := visited.set! y true
              q := q.enqueue (y, d+1)
  return ans

/-- Main IO routine. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let primes := sieve 9999
  for _ in [0:t] do
    let line := (← h.getLine).trim
    let parts := line.split (· = ' ') |>.filter (· ≠ "")
    let a := parts[0]! |>.toNat!
    let b := parts[1]! |>.toNat!
    match bfs a b primes with
    | some d => IO.println s!"{d}"
    | none   => IO.println "Impossible"
