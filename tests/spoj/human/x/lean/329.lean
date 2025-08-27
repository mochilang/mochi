/- Solution for SPOJ CALLS - Calls
https://www.spoj.com/problems/CALLS/
-/

import Std
open Std

/-- Parse a string of space-separated natural numbers. -/
private def parseNats (s : String) : Array Nat :=
  (s.split (· = ' ') |>.filter (· ≠ "") |>.map String.toNat!).toArray

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let header ← h.getLine
    let hk := parseNats header.trim
    let n := hk[0]!
    let k := hk[1]!
    -- read distance matrix
    let mut dist := Array.replicate n (Array.replicate n (0 : Nat))
    for i in [0:n-1] do
      let line ← h.getLine
      let nums := parseNats line.trim
      for j in [0:(n - i - 1)] do
        let d := nums[j]!
        let jIdx := i + j + 1
        dist := dist.set! i ((dist[i]!).set! jIdx d)
        dist := dist.set! jIdx ((dist[jIdx]!).set! i d)
    let totalPairs := n * (n - 1) / 2
    let mut needed := 0
    let mut ok := true
    for i in [0:n] do
      for j in [i+1:n] do
        let d := dist[i]![j]!
        let mut eq := false
        for m in [0:n] do
          if m ≠ i && m ≠ j then
            let val := dist[i]![m]! + dist[m]![j]!
            if val < d then
              ok := false
            else if val = d then
              eq := true
        if !eq then
          needed := needed + 1
    if ok && needed ≤ k && k ≤ totalPairs then
      IO.println "YES"
    else
      IO.println "NO"
    solve h (t - 1)


def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
