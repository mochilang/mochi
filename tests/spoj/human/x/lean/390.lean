/- Solution for SPOJ BILLIARD - Billiard
https://www.spoj.com/problems/BILLIARD/
-/

import Std
open Std

/-- round to two decimals --/
def round2 (x : Float) : Float :=
  Float.round (x * 100.0) / 100.0

/-- format with two decimals --/
def fmt (x : Float) : String :=
  let r := round2 x
  let s := r.toString
  if s.contains '.' then
    let parts := s.split (· = '.') |>.toArray
    let intPart := parts[0]!
    let frac := parts[1]!
    if frac.length = 1 then intPart ++ "." ++ frac ++ "0"
    else if frac.length = 2 then s
    else intPart ++ "." ++ frac.take 2
  else s ++ ".00"

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let line := line.trim
  if line.isEmpty then
    pure ()
  else
    let parts := line.split (fun c => c = ' ')
                  |>.filter (· ≠ "")
                  |>.toArray
    let a := parts[0]! |>.toNat!
    let b := parts[1]! |>.toNat!
    let s := parts[2]! |>.toNat!
    let m := parts[3]! |>.toNat!
    let n := parts[4]! |>.toNat!
    if a == 0 && b == 0 && s == 0 && m == 0 && n == 0 then
      pure ()
    else
      let dx := Float.ofNat a * Float.ofNat m
      let dy := Float.ofNat b * Float.ofNat n
      let angle := Float.atan2 dy dx * 180.0 / 3.141592653589793
      let speed := Float.sqrt (dx*dx + dy*dy) / Float.ofNat s
      IO.println (fmt angle ++ " " ++ fmt speed)
      loop h

def main : IO Unit := do
  loop (← IO.getStdin)
