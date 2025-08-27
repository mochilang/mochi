/- Solution for SPOJ CISTFILL - Fill the Cisterns
https://www.spoj.com/problems/CISTFILL/
-/

import Std
open Std

structure Cistern where
  base : Float
  top  : Float
  area : Float

def volumeAt (cs : Array Cistern) (level : Float) : Float :=
  cs.foldl (init := 0.0) (fun acc c =>
    let topLevel := if level < c.top then level else c.top
    let filled := topLevel - c.base
    let filled := if filled < 0.0 then 0.0 else filled
    acc + filled * c.area
  )

def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

partial def solveCases (toks : Array String) (idx t : Nat) (acc : Array String) :
    Array String :=
  if t = 0 then acc else
    let n := toks.get! idx |>.toNat!
    let mut cs : Array Cistern := #[]
    let mut i := idx + 1
    let mut maxTop : Float := 0.0
    let mut total : Float := 0.0
    for _ in [0:n] do
      let b := toks.get! i |>.toNat!
      let h := toks.get! (i+1) |>.toNat!
      let w := toks.get! (i+2) |>.toNat!
      let d := toks.get! (i+3) |>.toNat!
      i := i + 4
      let base := Float.ofNat b
      let top := Float.ofNat (b + h)
      let area := Float.ofNat (w * d)
      cs := cs.push ⟨base, top, area⟩
      total := total + area * Float.ofNat h
      if top > maxTop then
        maxTop := top
    let V := toks.get! i |>.toFloat!
    i := i + 1
    let ans :=
      if total < V then
        "OVERFLOW"
      else
        let mut lo := 0.0
        let mut hi := maxTop
        for _ in [0:60] do
          let mid := (lo + hi) / 2.0
          if volumeAt cs mid ≥ V then
            hi := mid
          else
            lo := mid
        format2 hi
    solveCases toks i (t - 1) (acc.push ans)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (· ≠ "") |> Array.ofList
  let k := toks.get! 0 |>.toNat!
  let outs := solveCases toks 1 k #[]
  for line in outs do
    IO.println line
