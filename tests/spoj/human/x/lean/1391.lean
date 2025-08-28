/- Solution for SPOJ CZ_PROB1 - Summing to a Square Prime
https://www.spoj.com/problems/CZ_PROB1/
-/

import Std
open Std

/-- Read all non-empty tokens from stdin as natural numbers. --/
def readInts : IO (Array Nat) := do
  let s ← IO.FS.Stream.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Generate primes `p ≤ limit` with `p = 2` or `p % 4 = 1`. --/
def squarePrimes (limit : Nat) : Array Nat := Id.run do
  let mut isPrime := Array.replicate (limit+1) true
  if limit ≥ 0 then isPrime := isPrime.set! 0 false
  if limit ≥ 1 then isPrime := isPrime.set! 1 false
  for i in [2:limit+1] do
    if isPrime[i]! then
      let mut j := i * i
      while j ≤ limit do
        isPrime := isPrime.set! j false
        j := j + i
  let mut res : Array Nat := #[]
  for i in [2:limit+1] do
    if isPrime[i]! then
      if i == 2 || i % 4 == 1 then
        res := res.push i
  return res

/-- Table where `table[k][n] = p(n, k)` for `k ≤ 3`. --/
def partTable (limit : Nat) : Array (Array Nat) := Id.run do
  let kmax := 3
  let mut res : Array (Array Nat) := Array.mkArray (kmax+1) (Array.mkArray (limit+1) 0)
  let mut dp := Array.mkArray (limit+1) 0
  dp := dp.set! 0 1
  res := res.set! 0 dp
  for k in [1:kmax+1] do
    for n in [k:limit+1] do
      let v := dp[n]! + dp[n - k]!
      dp := dp.set! n v
    res := res.set! k dp
  return res

-- precomputed sequences
def sqPrs : Array Nat := squarePrimes 8000

def parts : Array (Array Nat) := partTable 7993

def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then return
  let t := data[0]!
  let mut idx := 1
  let mut out : List String := []
  for _ in [0:t] do
    let n := data[idx]!; let k := data[idx+1]!
    idx := idx + 2
    let prime := sqPrs[n-1]!
    let ans := (parts[k]!)[prime]!
    out := toString ans :: out
  IO.println <| String.intercalate "\n" out.reverse
