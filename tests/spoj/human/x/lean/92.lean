/- Solution for SPOJ CUTSQRS - Cutting off Squares
https://www.spoj.com/problems/CUTSQRS/
-/

import Std
open Std

-- constants for log10(2) approximation
def logNum : Nat := 301029995663981
def logDen : Nat := 1000000000000000

-- number of decimal digits of 2^e
def digitsPow2 (e : Nat) : Nat :=
  e * logNum / logDen + 1

-- compute sum of quotients and count of steps in Euclidean algorithm
partial def sumQuotients (a b : Nat) : Nat × Nat :=
  let rec loop (a b sum n : Nat) : Nat × Nat :=
    if a == 0 then
      (sum, n)
    else
      let q := b / a
      loop (b % a) a (sum + q) (n + 1)
  loop a b 0 0

-- determine if starting player has a winning strategy
partial def firstWins (a b : Nat) : Bool :=
  let rec go (a b : Nat) (first : Bool) : Bool :=
    if b % a == 0 then first
    else if b / a >= 2 then first
    else go (b - a) a (!first)
  go a b true

-- process one test case
def solveCase (a b : Nat) : Nat :=
  let (s, n) := sumQuotients a b
  let exp := s - n
  let totalDigits := digitsPow2 exp
  let winDigits := digitsPow2 (exp - 1)
  let wordLen := if firstWins a b then 5 else 6
  totalDigits + winDigits + wordLen

partial def loopCases (h : IO.FS.Stream) (t idx : Nat) : IO Unit := do
  if idx == t then
    pure ()
  else
    let line ← h.getLine
    let parts := (line.trim.split (· = ' ')).filter (· ≠ "")
    let a := parts.get! 0 |>.toNat!
    let b := parts.get! 1 |>.toNat!
    IO.println (solveCase a b)
    loopCases h t (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := tLine.trim.toNat!
  loopCases h t 0
