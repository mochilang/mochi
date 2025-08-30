/- Solution for SPOJ RAIN2 - Outside it is now raining
https://www.spoj.com/problems/RAIN2/
-/

import Std
open Std

namespace SPOJ1468

def fmod (a b : Float) : Float :=
  a - Float.floor (a / b) * b

/-- position of umbrella left edge at time t --/
def pos (x l v t W : Float) : Float :=
  let L := W - l
  if L <= 0 then 0.0 else
    let modv := 2.0 * L
    let mut p := fmod (x + v * t) modv
    if p < 0.0 then
      p := p + modv
    if p > L then
      p := 2.0 * L - p
    p

/-- format number with exactly two decimals --/
def format2 (x : Float) : String :=
  let y := Float.floor (x * 100.0 + 0.5) / 100.0
  let s := y.toString
  let parts := s.split (· = '.')
  if parts.length = 1 then
    parts[0]! ++ ".00"
  else
    let intPart := parts[0]!
    let fracPart := parts[1]!
    let frac :=
      if fracPart.length ≥ 2 then fracPart.take 2
      else fracPart ++ String.mk (List.replicate (2 - fracPart.length) '0')
    intPart ++ "." ++ frac

def solveCase (W T V : Int) (umb : Array (Int×Int×Int)) : Float :=
  let steps : Nat := 100000
  let Wf := Float.ofInt W
  let Tf := Float.ofInt T
  let Vf := Float.ofInt V
  let dt := Tf / Float.ofNat steps
  let mut total : Float := 0.0
  for k in [0:steps] do
    let t := (Float.ofNat k + 0.5) * dt
    let mut segs : Array (Float×Float) := #[]
    for u in umb do
      let (x, (l, v)) := u
      let px := pos (Float.ofInt x) (Float.ofInt l) (Float.ofInt v) t Wf
      segs := segs.push (px, px + Float.ofInt l)
    let segs := segs.qsort (fun a b => a.fst < b.fst)
    let mut cover : Float := 0.0
    let mut curL : Float := 0.0
    let mut curR : Float := 0.0
    let mut first := true
    for s in segs do
      let a := s.fst
      let b := s.snd
      if first then
        first := false
        curL := a
        curR := b
      else if a ≤ curR then
        if b > curR then curR := b
      else
        cover := cover + (curR - curL)
        curL := a
        curR := b
    if !first then
      cover := cover + (curR - curL)
    total := total + (Wf - cover)
  total * dt * Vf

partial def solve (toks : Array String) : Array String := Id.run do
  let q := toks[0]!.toNat!
  let mut idx := 1
  let mut res : Array String := #[]
  for _ in [0:q] do
    let n := toks[idx]!.toNat!
    let W := toks[idx+1]!.toInt!
    let T := toks[idx+2]!.toInt!
    let V := toks[idx+3]!.toInt!
    idx := idx + 4
    let mut umb : Array (Int×Int×Int) := #[]
    for _ in [0:n] do
      let x := toks[idx]!.toInt!
      let l := toks[idx+1]!.toInt!
      let v := toks[idx+2]!.toInt!
      umb := umb.push (x, l, v)
      idx := idx + 3
    let ans := solveCase W T V umb
    res := res.push (format2 ans)
  return res

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let out := solve toks
  for line in out do
    IO.println line

end SPOJ1468
