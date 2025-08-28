/- Solution for SPOJ TRANSMIT - Transmitters
https://www.spoj.com/problems/TRANSMIT/
-/

import Std
open Std

def piF : Float := 3.141592653589793
def eps : Float := 1e-9

def maxCovered (cx cy r : Float) (pts : Array (Float × Float)) : Nat :=
  Id.run do
    let r2 := r * r
    let mut angs : Array Float := Array.mkEmpty pts.size
    for (x,y) in pts do
      let dx := x - cx
      let dy := y - cy
      if dx*dx + dy*dy <= r2 + eps then
        angs := angs.push (Float.atan2 dy dx)
    let n := angs.size
    if n == 0 then
      return 0
    let angs := angs.qsort (· < ·)
    let mut ext : Array Float := Array.mkEmpty (2*n)
    for a in angs do
      ext := ext.push a
    for a in angs do
      ext := ext.push (a + 2.0*piF)
    let mut ans : Nat := 0
    let mut j : Nat := 0
    for i in [0:n] do
      if j < i then
        j := i
      let limit := ext.get! i + piF + eps
      while j < i + n && ext.get! j <= limit do
        j := j + 1
      let cnt := j - i
      if cnt > ans then
        ans := cnt
    return ans

partial def readPts (toks : Array String) (idx n : Nat) (acc : Array (Float × Float)) :
    (Array (Float × Float) × Nat) :=
  if n = 0 then
    (acc, idx)
  else
    let x := toks.get! idx |>.toFloat!
    let y := toks.get! (idx+1) |>.toFloat!
    readPts toks (idx+2) (n-1) (acc.push (x,y))

partial def solveAll (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  if idx + 2 ≥ toks.size then
    acc.reverse
  else
    let cx := toks.get! idx |>.toFloat!
    let cy := toks.get! (idx+1) |>.toFloat!
    let r := toks.get! (idx+2) |>.toFloat!
    if r < 0 then
      acc.reverse
    else
      let n := toks.get! (idx+3) |>.toNat!
      let (pts, idx') := readPts toks (idx+4) n #[]
      let ans := maxCovered cx cy r pts
      solveAll toks idx' (toString ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |> Array.ofList
  let outs := solveAll toks 0 []
  for line in outs do
    IO.println line
