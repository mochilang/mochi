/- Solution for SPOJ PLONK - Where to Drink the Plonk?
https://www.spoj.com/problems/PLONK/
-/

import Std
open Std

-- absolute value for Int
@[inline] def absInt (x : Int) : Int := if x < 0 then -x else x

partial def process (h : IO.FS.Stream) (cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut xs : Array Int := Array.mkEmpty n
    let mut ys : Array Int := Array.mkEmpty n
    for _ in [:n] do
      let line := (← h.getLine).trim
      let parts := line.split (fun c => c = ' ') |>.filter (· ≠ "")
      let xStr := parts.getD 0 "0"
      let yStr := parts.getD 1 "0"
      xs := xs.push xStr.toInt!
      ys := ys.push yStr.toInt!
    let mut xsSorted := xs
    let mut ysSorted := ys
    xsSorted := xsSorted.qsort (· < ·)
    ysSorted := ysSorted.qsort (· < ·)
    let m := n / 2
    let mx := xsSorted.get! m
    let my := ysSorted.get! m
    let mut total : Int := 0
    for i in [:n] do
      total := total + absInt (xs.get! i - mx) + absInt (ys.get! i - my)
    IO.println total
    process h (cases - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
