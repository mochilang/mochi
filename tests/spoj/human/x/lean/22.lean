/- Solution for SPOJ TRICENTR - Triangle and Centroid
https://www.spoj.com/problems/TRICENTR/
-/

import Std
open Std

private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

private def solveCase (a ga gb gc : Float) : String :=
  let area := 1.5 * a * ga
  let b := a * ga / gb
  let c := a * ga / gc
  let r := a * b * c / (4.0 * area)
  let s2 := a*a + b*b + c*c
  let og2 := r*r - s2 / 9.0
  let gh :=
    if og2 ≤ 0.0 then
      0.0
    else
      2.0 * Float.sqrt og2
  format3 area ++ " " ++ format3 gh

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) :
    List String :=
  if t = 0 then acc.reverse else
    let a := toks.get! idx |>.toFloat!
    let ga := toks.get! (idx+1) |>.toFloat!
    let gb := toks.get! (idx+2) |>.toFloat!
    let gc := toks.get! (idx+3) |>.toFloat!
    let out := solveCase a ga gb gc
    solveAll toks (idx+4) (t-1) (out :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (fun s => s ≠ "")
                  |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 t []
  for line in outs do
    IO.println line
