/- Solution for SPOJ PHIDIAS - Phidias
https://www.spoj.com/problems/PHIDIAS/
-/
import Std
open Std

def solveCase (W H : Nat) (plates : Array (Nat × Nat)) : Nat :=
  Id.run do
    let mut dp : Array (Array Nat) := Array.replicate (W + 1) (Array.replicate (H + 1) 0)
    for (w, h) in plates do
      if w ≤ W ∧ h ≤ H then
        let row := dp.get! w
        dp := dp.set! w (row.set! h (w * h))
    for w in [1:W+1] do
      for h in [1:H+1] do
        let mut best := (dp.get! w).get! h
        for x in [1:w] do
          let v := (dp.get! x).get! h + (dp.get! (w - x)).get! h
          if v > best then
            best := v
        for y in [1:h] do
          let v := (dp.get! w).get! y + (dp.get! w).get! (h - y)
          if v > best then
            best := v
        let row := dp.get! w
        dp := dp.set! w (row.set! h best)
    let used := (dp.get! W).get! H
    return W * H - used

partial def processCases (h : IO.FS.Stream) (t idx : Nat) : IO Unit := do
  if idx = t then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      processCases h t idx
    else
      let parts := line.splitOn " "
      let W := (parts.get! 0).toNat!
      let H := (parts.get! 1).toNat!
      let n := (← h.getLine).trim.toNat!
      let mut plates : Array (Nat × Nat) := Array.mkEmpty n
      for _ in [0:n] do
        let ln := (← h.getLine).trim
        let ps := ln.splitOn " "
        plates := plates.push ((ps.get! 0).toNat!, (ps.get! 1).toNat!)
      let waste := solveCase W H plates
      IO.println waste
      processCases h t (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  processCases h t 0
