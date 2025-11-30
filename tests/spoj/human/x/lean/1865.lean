/- Solution for SPOJ MKWAVES - Making Waves
https://www.spoj.com/problems/MKWAVES/
-/

import Std
open Std

-- compute sine of angle given in degrees
@[inline] def sinDeg (x : Float) : Float :=
  Float.sin (x * Float.pi / 180.0)

-- solve a single test case
partial def solveCase (caseId : Nat) (n : Nat) (samples : Array Float) : String :=
  let freqCount := 201 -- frequencies 400..600
  let mut sinTable : Array (Array Float) := Array.mkArray freqCount (Array.mkArray n 0.0)
  let mut dot : Array Float := Array.mkArray freqCount 0.0  -- Σ sample * sin
  let mut self : Array Float := Array.mkArray freqCount 0.0 -- Σ sin^2
  -- precompute sin values and correlations
  for fIdx in [0:freqCount] do
    let freq := Float.ofNat (400 + fIdx)
    let mut arr : Array Float := Array.mkArray n 0.0
    let mut a : Float := 0.0
    let mut b : Float := 0.0
    for i in [0:n] do
      let t := Float.ofNat (i + 1) / Float.ofNat n
      let v := sinDeg (freq * t)
      arr := arr.set! i v
      let s := samples.get! i
      a := a + s * v
      b := b + v * v
    sinTable := sinTable.set! fIdx arr
    dot := dot.set! fIdx a
    self := self.set! fIdx b
  -- sum of squares of samples
  let mut s2 : Float := 0.0
  for x in samples do
    s2 := s2 + x*x
  -- cross correlation matrix
  let mut cross : Array (Array Float) := Array.mkArray freqCount (Array.mkArray freqCount 0.0)
  for fIdx in [0:freqCount] do
    for gIdx in [fIdx+1:freqCount] do
      let arrF := sinTable.get! fIdx
      let arrG := sinTable.get! gIdx
      let mut c : Float := 0.0
      for i in [0:n] do
        c := c + (arrF.get! i) * (arrG.get! i)
      cross := cross.set! fIdx ((cross.get! fIdx).set! gIdx c)
      cross := cross.set! gIdx ((cross.get! gIdx).set! fIdx c)
  -- search best pair
  let mut bestF := 0
  let mut bestG := 1
  let mut bestErr : Float := Float.infinity
  for fIdx in [0:freqCount] do
    for gIdx in [fIdx+1:freqCount] do
      let err := s2 + (self.get! fIdx) + (self.get! gIdx)
        + 2.0 * ((cross.get! fIdx).get! gIdx)
        - 2.0 * (dot.get! fIdx) - 2.0 * (dot.get! gIdx)
      if err < bestErr then
        bestErr := err
        bestF := fIdx
        bestG := gIdx
  let f1 := 400 + bestF
  let f2 := 400 + bestG
  s!"Case {caseId}, f1 = {f1}, f2 = {f2}"

partial def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let toks := input.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r').filter (· ≠ "")
  let arr : Array String := toks.toArray
  let mut idx : Nat := 0
  let next := fun () =>
    let t := arr.get! idx
    idx := idx + 1
    t
  let mut caseId := 1
  while true do
    let nStr := next()
    let n := nStr.toNat!
    if n = 0 then
      break
    let mut samples : Array Float := Array.mkArray n 0.0
    for i in [0:n] do
      samples := samples.set! i (Float.ofString! (next()))
    let out := solveCase caseId n samples
    IO.println out
    caseId := caseId + 1
