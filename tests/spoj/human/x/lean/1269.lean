/- Solution for SPOJ CNHARD - CN Tower (Hard)
https://www.spoj.com/problems/CNHARD/
-/

import Std
open Std

-- parse angle string with up to two decimals into hundredths of degree
private def parseAngle (s : String) : Int :=
  let parts := s.splitOn "."
  let intPart := (parts.get! 0).toNat!
  let fracPart :=
    if h : parts.length > 1 then
      let frac := parts.get! 1
      let frac2 := (frac ++ "00").take 2
      frac2.toNat!
    else
      0
  Int.ofNat (intPart * 100 + fracPart)

-- compute minimal required time in units of 1/25 seconds
private def solveCase (angles : Array Int) (flash : Int) : Int :=
  let sorted := angles.qsort (· < ·)
  let n := sorted.size
  let flashU := flash * 25
  let rot := 108000 -- one full rotation in units (4320s * 25)
  let mut best := (216000 : Int) + 1 -- closing time + 1
  for i in [0:n] do
    let offset := sorted.get! i
    let mut last : Int := -flashU
    for j in [0:n] do
      let idx := (i + j) % n
      let a0 := sorted.get! idx
      let a := if a0 < offset then a0 + 36000 else a0
      let tAngle := (a - offset) * 3
      let mut t := tAngle
      let need := last + flashU
      if t < need then
        let k := (need - t + rot - 1) / rot
        t := t + rot * k
      last := t
    let total := last + flashU
    if total < best then
      best := total
  best

private def process (toks : Array String) : Array String := Id.run do
  let t := toks.get! 0 |>.toNat!
  let mut idx := 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := toks.get! idx |>.toNat!; idx := idx + 1
    let mut angs : Array Int := #[]
    for _ in [0:n] do
      let _name := toks.get! idx; idx := idx + 1
      let ang := parseAngle (toks.get! idx); idx := idx + 1
      angs := angs.push ang
    let flash := toks.get! idx |>.toNat!; idx := idx + 1
    let total := solveCase angs (Int.ofNat flash)
    let close := 216000
    let out := if total > close then "not possible"
               else toString ((total + 24) / 25)
    outs := outs.push out
  outs

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let res := process toks
  for line in res do
    IO.println line
