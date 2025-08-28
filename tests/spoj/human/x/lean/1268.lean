/- Solution for SPOJ CNEASY - CN Tower (Easy)
https://www.spoj.com/problems/CNEASY/
-/

import Std
open Std

/-- Parse an angle like "123.45" into hundredths of degrees. -/
def parseAngle (s : String) : Nat :=
  let parts := s.splitOn "."
  let intPart := parts.head!.toNat!
  let fracPart :=
    match parts.tail with
    | []      => 0
    | h :: _ => ((h ++ "00").take 2).toNat!
  intPart * 100 + fracPart

/-- Compute minimal time in seconds to see all angles (given in hundredths of degrees). -/
def minTime (angles : Array Nat) : Nat :=
  Id.run do
    let sorted := angles.qsort (· < ·)
    let n := sorted.size
    let mut maxGap : Nat := 0
    for i in [0:n] do
      let curr := sorted.get! i
      let next := if h : i+1 < n then sorted.get! (i+1) else sorted.get! 0 + 36000
      let gap := next - curr
      maxGap := Nat.max maxGap gap
    let span := 36000 - maxGap
    return (span * 12 + 99) / 100

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List Nat) : List Nat :=
  if t = 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let (angles, pos) := Id.run do
      let mut angles : Array Nat := #[]
      let mut pos := idx + 1
      for _ in [0:n] do
        let _name := toks.get! pos
        let a := parseAngle (toks.get! (pos+1))
        angles := angles.push a
        pos := pos + 2
      return (angles, pos)
    let ans := minTime angles
    solveAll toks pos (t-1) (ans :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
              |>.filter (· ≠ "")
              |> List.toArray
  if toks.size = 0 then
    return ()
  let t := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 t []
  for v in outs do
    IO.println v
