/- Solution for SPOJ EASYPIE - Easy Problem
https://www.spoj.com/problems/EASYPIE/
-/

import Std
open Std

structure Info where
  solved : Bool := false
  subs : Nat := 0
  time : Nat := 0

def initInfos : Array Info := Array.mkArray 9 {}

private def format2 (x : Float) : String :=
  let y := if x >= 0.0 then x + 0.005 else x - 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart :=
    match parts with
    | p :: _ => if p == "-0" then "0" else p
    | [] => "0"
  let fracPart :=
    match parts.drop 1 with
    | f :: _ => (f ++ "00").take 2
    | [] => "00"
  intPart ++ "." ++ fracPart

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "") |> Array.ofList
  let mut idx : Nat := 0
  let t := toks.get! idx |>.toNat!
  idx := idx + 1
  for _ in [:t] do
    let n := toks.get! idx |>.toNat!
    idx := idx + 1
    let mut teams : HashMap String (Array Info) := HashMap.empty
    for _ in [:n] do
      let time := toks.get! idx |>.toNat!
      idx := idx + 1
      let team := toks.get! idx
      idx := idx + 1
      let prob := toks.get! idx
      idx := idx + 1
      let res := toks.get! idx
      idx := idx + 1
      let pIdx := (prob.data.get! 0).toNat - 'A'.toNat
      let r := res.data.get! 0
      let arr := teams.findD team initInfos
      let info := arr.get! pIdx
      if info.solved then
        teams := teams.insert team arr
      else
        let info := { info with subs := info.subs + 1 }
        let info := if r == 'A' then { info with solved := true, time := time } else info
        let arr := arr.set! pIdx info
        teams := teams.insert team arr
    let mut counts := Array.mkArray 9 0
    let mut sumSubs := Array.mkArray 9 0
    let mut sumTimes := Array.mkArray 9 0
    for (_, arr) in teams.toList do
      for j in [:9] do
        let info := arr.get! j
        if info.solved then
          counts := counts.set! j (counts.get! j + 1)
          sumSubs := sumSubs.set! j (sumSubs.get! j + info.subs)
          sumTimes := sumTimes.set! j (sumTimes.get! j + info.time)
    for j in [:9] do
      let c := counts.get! j
      let ch := Char.ofNat ('A'.toNat + j)
      if c = 0 then
        IO.println s!"{ch} 0"
      else
        let avgSub := Float.ofNat (sumSubs.get! j) / Float.ofNat c
        let avgTime := Float.ofNat (sumTimes.get! j) / Float.ofNat c
        IO.println s!"{ch} {c} {format2 avgSub} {format2 avgTime}"
