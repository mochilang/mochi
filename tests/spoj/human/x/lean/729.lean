/- Solution for SPOJ MAXIMUS - Move your armies
https://www.spoj.com/problems/MAXIMUS/
-/

import Std
open Std

partial def isPrimeAux (n d : Nat) : Bool :=
  if d * d > n then true
  else if n % d == 0 then false
  else isPrimeAux n (d + 1)

def isPrime (n : Nat) : Bool :=
  if n <= 1 then true else isPrimeAux n 2

def moves (n : Nat) : Nat :=
  let rec loop : Nat → Nat → Nat → Nat
  | 0, r, l => l
  | m+1, r, l =>
      if isPrime (m+1) then
        let newR := 2 * l + 1
        let newL := 2 * l + r + 2
        loop m newR newL
      else
        let newL := 2 * r + 1
        let newR := 2 * r + l + 2
        loop m newR newL
  loop n 0 0

partial def process : List String → IO Unit
  | [] => pure ()
  | s :: ss =>
      let n := s.trim.toNat!
      if n == 0 then
        pure ()
      else
        IO.println (moves n)
        process ss

def main : IO Unit := do
  let lines := (← IO.readStdin).trim.split (· = '\n')
  process lines
