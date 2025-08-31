/- Solution for SPOJ DETER2 - Find The Determinant II
https://www.spoj.com/problems/DETER2/
-/

import Std
open Std

def MOD : Nat := 1000003

partial def powMod (a b : Nat) : Nat :=
  if b == 0 then 1 % MOD
  else
    let h := powMod a (b / 2)
    let hh := (h * h) % MOD
    if b % 2 == 0 then hh else (hh * (a % MOD)) % MOD

def modInv (a : Nat) : Nat :=
  powMod a (MOD - 2)

def sieve (n : Nat) : Array Nat := Id.run do
  let mut isPrime := Array.replicate (n + 1) true
  if n >= 0 then
    isPrime := isPrime.set! 0 false
  if n >= 1 then
    isPrime := isPrime.set! 1 false
  let mut res : Array Nat := #[]
  for i in [2:n+1] do
    if isPrime[i]! then
      res := res.push i
      let mut j := i * i
      while j <= n do
        isPrime := isPrime.set! j false
        j := j + i
  return res

def factorials (n : Nat) : Array Nat := Id.run do
  let mut arr := Array.mkArray (n + 1) 1
  for i in [1:n+1] do
    let v := (arr.get! (i - 1) * i) % MOD
    arr := arr.set! i v
  return arr

def readNats : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

def solve (n k : Nat) (facts : Array Nat) (primes : Array Nat) : Nat :=
  let mut res := powMod (facts.get! n) k
  for p in primes do
    if p <= n then
      let c := n / p
      let pPow := powMod p k
      let term := ((pPow + MOD - 1) % MOD) * modInv pPow % MOD
      let termPow := powMod term c
      res := (res * termPow) % MOD
  res

def main : IO Unit := do
  let data ← readNats
  if data.size = 0 then return ()
  let t := data.get! 0
  let mut idx := 1
  let mut cases : Array (Nat × Nat) := #[]
  let mut maxN : Nat := 0
  for _ in [0:t] do
    let n := data.get! idx; idx := idx + 1
    let k := data.get! idx; idx := idx + 1
    cases := cases.push (n, k)
    if n > maxN then maxN := n
  let facts := factorials maxN
  let primes := sieve maxN
  for (n, k) in cases do
    IO.println (solve n k facts primes)
