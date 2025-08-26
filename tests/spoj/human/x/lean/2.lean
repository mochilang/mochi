/- Solution for SPOJ PRIME1 - Prime Generator
https://www.spoj.com/problems/PRIME1/
-/

import Std
open Std

partial def isPrimeAux (n d : Nat) : Bool :=
  if d * d > n then true
  else if n % d == 0 then false
  else isPrimeAux n (d + 1)

def isPrime (n : Nat) : Bool :=
  if n < 2 then false else isPrimeAux n 2

partial def printPrimesFrom (m n : Nat) : IO Unit := do
  if m > n then
    pure ()
  else
    if isPrime m then
      IO.println m
    printPrimesFrom (m + 1) n

partial def processCases (h : IO.FS.Stream) (t : Nat) (idx : Nat) : IO Unit := do
  if idx == t then
    pure ()
  else
    let line ← h.getLine
    let parts := (line.trim.split (· = ' ')).filter (· ≠ "")
    let m := parts.get! 0 |>.toNat!
    let n := parts.get! 1 |>.toNat!
    printPrimesFrom m n
    if idx < t - 1 then
      IO.println ""
    processCases h t (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := tLine.trim.toNat!
  processCases h t 0
