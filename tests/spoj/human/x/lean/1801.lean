/- Solution for SPOJ DRINK - Drink, on Ice
https://www.spoj.com/problems/DRINK/
-/

import Std
open Std

private def cw : Float := 4.19
private def ci : Float := 2.09
private def em : Float := 335.0

private def format1 (x : Float) : String :=
  let r := (Float.round (x * 10.0)) / 10.0
  let s := r.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      (parts.get! 1 ++ "0").take 1
    else
      "0"
  intPart ++ "." ++ fracPart

private def solve (mw mi tw ti : Float) : (Float × Float × Float) :=
  let heatIce := mi * ci * (-ti)
  let coolWater := mw * cw * tw
  if coolWater > heatIce then
    let leftover := coolWater - heatIce
    let meltAll := mi * em
    if leftover >= meltAll then
      let temp := (leftover - meltAll) / ((mw + mi) * cw)
      (0.0, mw + mi, temp)
    else
      let melted := leftover / em
      (mi - melted, mw + melted, 0.0)
  else if coolWater == heatIce then
    (mi, mw, 0.0)
  else
    let temp := (mw*cw*tw + mw*em + mi*ci*ti) / ((mw + mi) * ci)
    (mw + mi, 0.0, temp)

partial def parseCases (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  if h : idx + 3 < toks.size then
    let mw := (toks.get! idx).toFloat!
    let mi := (toks.get! (idx+1)).toFloat!
    let tw := (toks.get! (idx+2)).toFloat!
    let ti := (toks.get! (idx+3)).toFloat!
    if mw = 0 && mi = 0 && tw = 0 && ti = 0 then
      acc.reverse
    else
      let (ice, water, temp) := solve mw mi tw ti
      let line := s!"{format1 ice} g of ice and {format1 water} g of water at {format1 temp} C"
      parseCases toks (idx + 4) (line :: acc)
  else
    acc.reverse

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let outs := parseCases toks 0 []
  for line in outs do
    IO.println line
