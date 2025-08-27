/- Solution for SPOJ PHDISP - The Philosophical Dispute
https://www.spoj.com/problems/PHDISP/
-/

import Std
open Std

def piF : Float := 3.141592653589793

/-- Format a float to one decimal place. -/
def format1 (x : Float) : String :=
  let n := (Float.floor (x * 10.0 + 0.5)).toUInt64.toNat
  let whole := n / 10
  let frac := n % 10
  s!"{whole}.{frac}"

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    if line.trim.isEmpty then
      loop h t
    else
      let parts := line.trim.splitOn " "
      let y0 := (parts.get! 1).toFloat!
      let _eps ← h.getLine
      let tval := Float.asin y0 + 1.5 * piF
      IO.println (format1 tval)
      loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
