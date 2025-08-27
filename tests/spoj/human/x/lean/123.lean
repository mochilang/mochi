/- Solution for SPOJ PAYING - Paying in Byteland
https://www.spoj.com/problems/PAYING/
-/

import Std
open Std

/-- largest power of two not exceeding `x` -/
def largestPow2LE (x : Nat) : Nat :=
  Nat.pow 2 (Nat.log2 x)

/-- minimal number of coins for a perfect representation of `n` -/
partial def minCoins (n : Nat) : Nat :=
  let rec loop (s cnt : Nat) :=
    if s < n then
      let r := n - s
      let m := Nat.min r (s + 1)
      let v := largestPow2LE m
      loop (s + v) (cnt + 1)
    else
      cnt
  loop 0 0

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (minCoins n)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
