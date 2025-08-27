/- Solution for SPOJ RENT - Rent your airplane and make money
https://www.spoj.com/problems/RENT/
-/

import Std
open Std

structure Order where
  start : Nat
  finish : Nat
  price : Nat
deriving Repr, Inhabited

/-- Compute maximum profit from non-overlapping orders -/
def maxProfit (orders : Array Order) : Nat :=
  Id.run do
    let sorted := orders.qsort (fun a b => a.finish < b.finish)
    let n := sorted.size
    let mut dp : Array Nat := Array.replicate (n + 1) 0
    for i in [1:n+1] do
      let ord := sorted[i-1]!
      -- binary search for last compatible order
      let mut lo := 0
      let mut hi := i - 1
      let mut res := 0
      while lo < hi do
        let mid := (lo + hi) / 2
        if (sorted[mid]!).finish <= ord.start then
          res := mid + 1
          lo := mid + 1
        else
          hi := mid
      let include := dp[res]! + ord.price
      let exclude := dp[i-1]!
      dp := dp.set! i (Nat.max include exclude)
    return dp[n]!

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut arr : Array Order := #[]
    for _ in [0:n] do
      let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
      let st := parts[0]! |>.toNat!
      let d := parts[1]! |>.toNat!
      let p := parts[2]! |>.toNat!
      arr := arr.push {start := st, finish := st + d, price := p}
    IO.println (maxProfit arr)
    solve h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
