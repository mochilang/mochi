/- Solution for SPOJ PARTPALI - Particular Palindromes
https://www.spoj.com/problems/PARTPALI/
-/

import Std
open Std

/-- Count s-digit palindromes divisible by m. -/
def countPal (m s : Nat) : Nat :=
  let half := s / 2
  Id.run do
    let mut pow := Array.mkArray (s+1) 0
    let mut cur := (1 % m)
    for i in [0:s+1] do
      pow := pow.set! i cur
      cur := (cur * 10) % m
    let mut dp := Array.mkArray m 0
    dp := dp.set! 0 1
    for i in [0:half] do
      let mut next := Array.mkArray m 0
      let p := (pow[s-1-i]! + pow[i]!) % m
      let start := if i == 0 then 1 else 0
      for r in [0:m] do
        let cnt := dp[r]!
        if cnt != 0 then
          for d in [start:10] do
            let nm := (r + (d * p) % m) % m
            next := next.set! nm (next[nm]! + cnt)
      dp := next
    if s % 2 == 1 then
      let mut next := Array.mkArray m 0
      let mid := pow[half]!
      let start := if half == 0 then 1 else 0
      for r in [0:m] do
        let cnt := dp[r]!
        if cnt != 0 then
          for d in [start:10] do
            let nm := (r + (d * mid) % m) % m
            next := next.set! nm (next[nm]! + cnt)
      dp := next
    return dp[0]!

/-- Entry point: read test cases and print counts. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let line ← h.getLine
    let parts := line.trim.split (· = ' ')
    let m := parts.get! 0 |>.toNat!
    let s := parts.get! 1 |>.toNat!
    IO.println (countPal m s)
