/- Solution for SPOJ PIE - Pie
https://www.spoj.com/problems/PIE/
-/

import Std
open Std

private def pi : Float := 3.14159265358979323846

private def countPieces (vols : List Float) (size : Float) : Nat :=
  vols.foldl (fun acc v => acc + (Float.floor (v / size)).toUInt64.toNat) 0

private def solveCase (vols : List Float) (need : Nat) : Float :=
  Id.run <| do
    let mut lo : Float := 0.0
    let mut hi : Float := 0.0
    for v in vols do
      if v > hi then
        hi := v
    for _ in [0:60] do
      let mid := (lo + hi) / 2.0
      if countPieces vols mid >= need then
        lo := mid
      else
        hi := mid
    return lo

private def format4 (x : Float) : String :=
  let y := x + 0.00005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "0000").take 4
    else
      "0000"
  intPart ++ "." ++ fracPart

def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  return ((s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun x => x ≠ "")).toArray

def main : IO Unit := do
  let toks ← readTokens
  let mut idx := 0
  let t := (toks[idx]!).toNat!; idx := idx + 1
  let mut outs : List String := []
  for _ in [0:t] do
    let n := (toks[idx]!).toNat!; idx := idx + 1
    let f := (toks[idx]!).toNat!; idx := idx + 1
    let mut vols : List Float := []
    for _ in [0:n] do
      let r := (toks[idx]!).toNat!; idx := idx + 1
      let v := pi * (Float.ofNat r) * (Float.ofNat r)
      vols := v :: vols
    let ans := solveCase vols (f + 1)
    outs := (format4 ans) :: outs
  for line in outs.reverse do
    IO.println line
