/- Solution for SPOJ PRINT - Prime Intervals
https://www.spoj.com/problems/PRINT/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.FS.Stream.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Simple sieve to generate primes up to `n`. --/
def sieve (n : Nat) : Array Nat := Id.run do
  let mut isPrime := Array.replicate (n+1) true
  isPrime := isPrime.set! 0 false
  if n ≥ 1 then
    isPrime := isPrime.set! 1 false
  let mut res : Array Nat := #[]
  for i in [2:n+1] do
    if isPrime[i]! then
      res := res.push i
      let mut j := i*i
      while j ≤ n do
        isPrime := isPrime.set! j false
        j := j + i
  return res

/-- Segmented sieve on interval `[l, u]` using precomputed `primes`. --/
def segment (l u : Nat) (primes : Array Nat) : Array Nat := Id.run do
  let size := u - l + 1
  let mut isPrime := Array.replicate size true
  for p in primes do
    if p*p ≤ u then
      let mut start := if l % p == 0 then l else l + (p - l % p)
      if start < p*p then
        start := p*p
      let mut j := start
      while j ≤ u do
        isPrime := isPrime.set! (j - l) false
        j := j + p
  if l == 1 then
    isPrime := isPrime.set! 0 false
  let mut res : Array Nat := #[]
  for i in [0:size] do
    if isPrime[i]! then
      res := res.push (l + i)
  return res

def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then return
  let t := data[0]!
  let primes := sieve 46340
  let mut idx := 1
  for case in [0:t] do
    let l := data[idx]!; let u := data[idx+1]!
    idx := idx + 2
    let seg := segment l u primes
    for p in seg do
      IO.println p
    if case + 1 < t then
      IO.println ""
