/- Solution for SPOJ CRSCNTRY - Cross-country
https://www.spoj.com/problems/CRSCNTRY/
-/

import Std
open Std

-- parse a line into an array of checkpoint numbers (excluding trailing 0)
private def parseRoute (line : String) : Array Nat :=
  let nums := line.split (· = ' ')
                |>.filter (fun s => s ≠ "")
                |>.map (fun s => s.toNat!)
  let nums := nums.takeWhile (fun n => n ≠ 0)
  nums.toArray

-- longest common subsequence length between two routes
private def lcs (a b : Array Nat) : Nat :=
  Id.run do
    let m := b.size
    let mut prev := Array.replicate (m+1) 0
    let mut curr := Array.replicate (m+1) 0
    for x in a do
      for j in [0:m] do
        let v :=
          if x = b[j]! then
            prev[j]! + 1
          else
            Nat.max (prev[j+1]!) (curr[j]!)
        curr := curr.set! (j+1) v
      prev := curr
      curr := Array.replicate (m+1) 0
    return prev[m]!

partial def handleTom (h : IO.FS.Stream) (agnes : Array Nat) (best : Nat) : IO Nat := do
  let line := (← h.getLine).trim
  if line = "0" then
    pure best
  else
    let route := parseRoute line
    let score := lcs agnes route
    handleTom h agnes (Nat.max best score)

partial def process (h : IO.FS.Stream) : Nat -> IO Unit
| 0 => pure ()
| Nat.succ t => do
    let agnes := parseRoute (← h.getLine).trim
    let best ← handleTom h agnes 0
    IO.println best
    process h t

def main : IO Unit := do
  let h ← IO.getStdin
  let d := (← h.getLine).trim.toNat!
  process h d
