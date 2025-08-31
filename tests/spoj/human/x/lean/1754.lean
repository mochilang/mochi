/- Solution for SPOJ DIVSUM2 - Divisor Summation (Hard)
https://www.spoj.com/problems/DIVSUM2/
-/

import Std
open Std

partial def sumProperDivisors (n : Nat) : Nat :=
  if n = 1 then
    0
  else
    let rec loop (m i res : Nat) : Nat :=
      if i * i > m then
        if m > 1 then res * (1 + m) else res
      else if m % i == 0 then
        let rec factor (m term sum : Nat) : Nat × Nat :=
          if m % i == 0 then
            let term' := term * i
            factor (m / i) term' (sum + term')
          else
            (m, sum)
        let (m', sum') := factor m 1 1
        loop m' (i + 1) (res * sum')
      else
        loop m (i + 1) res
    (loop n 2 1) - n

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (sumProperDivisors n)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
