/- Solution for SPOJ PARTY - Party Schedule
https://www.spoj.com/problems/PARTY/
-/
import Std
open Std

partial def readNonEmpty (h : IO.FS.Stream) : IO String := do
  let line ← h.getLine
  let s := line.trim
  if s.isEmpty then readNonEmpty h else pure s

partial def solve (h : IO.FS.Stream) : IO Unit := do
  let line := (← readNonEmpty h)
  let parts := line.splitOn " "
  let budget := parts.get! 0 |>.toNat!
  let n := parts.get! 1 |>.toNat!
  if budget = 0 && n = 0 then
    pure ()
  else
    let mut dp := Array.mkArray (budget + 1) 0
    for _ in [0:n] do
      let ln := (← readNonEmpty h)
      let ps := ln.splitOn " "
      let cost := ps.get! 0 |>.toNat!
      let fun := ps.get! 1 |>.toNat!
      for i in [0:budget+1] do
        let w := budget - i
        if cost ≤ w then
          let old := dp.get! w
          let cand := dp.get! (w - cost) + fun
          if cand > old then
            dp := dp.set! w cand
    let mut maxFun := 0
    let mut minCost := 0
    for w in [0:budget+1] do
      let f := dp.get! w
      if f > maxFun then
        maxFun := f
        minCost := w
      else if f = maxFun && w < minCost then
        minCost := w
    IO.println s!"{minCost} {maxFun}"
    solve h

def main : IO Unit := do
  let h ← IO.getStdin
  solve h
