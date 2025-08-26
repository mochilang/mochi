/- Solution for SPOJ MARBLES - Marbles
https://www.spoj.com/problems/MARBLES/
-/

import Std
open Std

-- compute binomial coefficient C(n,k)
def choose (n k : Nat) : Nat :=
  let k := if k > n - k then n - k else k
  let mut res := 1
  for i in [1:k+1] do
    res := res * (n - k + i) / i
  res

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine)
    let parts := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let n := parts.get! 0 |>.toNat!
    let k := parts.get! 1 |>.toNat!
    IO.println (choose (n - 1) (k - 1))
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
