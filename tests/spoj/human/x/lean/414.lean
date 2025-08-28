/- Solution for SPOJ BONFIRE - Equatorial Bonfire
https://www.spoj.com/problems/BONFIRE/
-/

import Std
open Std

structure Fire where
  t : Float
  x : Float
deriving Inhabited

-- parse float from string
private def parseFloat (s : String) : Float :=
  let sign : Float := if s.startsWith "-" then -1.0 else 1.0
  let s := if sign < 0 then s.drop 1 else s
  let parts := s.splitOn "."
  let intPart :=
    match parts with
    | p :: _ => p.toNat!
    | [] => 0
  let fracPart :=
    match parts.drop 1 with
    | f :: _ =>
        let n := f.toNat!
        let denom := Float.ofNat (Nat.pow 10 f.length)
        Float.ofNat n / denom
    | [] => 0.0
  sign * (Float.ofNat intPart + fracPart)

-- normalize position to [0,360)
private def norm (x : Float) : Float :=
  let y := x - Float.floor (x / 360.0) * 360.0
  let y := if y < 0.0 then y + 360.0 else y
  y

-- circular distance in degrees
private def dist (a b : Float) : Float :=
  let diff := Float.abs (norm a - norm b)
  if diff > 180.0 then 360.0 - diff else diff

-- check if at time T the fires cover entire circle
private def covered (fires : Array Fire) (v T : Float) : Bool :=
  let intervals := fires.foldl (init := #[]) fun acc f =>
    if T < f.t then acc else
    let r := v * (T - f.t)
    let lRaw := f.x - r
    let rRaw := f.x + r
    if r ≥ 180.0 then acc.push (0.0, 360.0) else
    let l := norm lRaw
    let rpos := norm rRaw
    if lRaw < 0.0 || rRaw ≥ 360.0 then (acc.push (0.0, rpos)).push (l, 360.0)
    else acc.push (l, rpos)
  if intervals.isEmpty then
    false
  else
    let sorted := intervals.qsort (fun a b => a.fst < b.fst)
    let first := sorted[0]!
    if first.fst > 0.0 then false else
    let rec loop (i : Nat) (endp : Float) : Bool :=
      if h : i < sorted.size then
        let s := sorted[i]!.fst
        let e := sorted[i]!.snd
        if s > endp + 1e-7 then
          false
        else
          loop (i+1) (if endp < e then e else endp)
      else
        endp >= 360.0 - 1e-6
    loop 1 first.snd

-- compute arrival time at position x
private def arrival (fires : Array Fire) (v x : Float) : Float :=
  fires.foldl (init := 1e100) fun best f =>
    let t := f.t + dist f.x x / v
    if t < best then t else best

-- format float with three decimals
private def format3 (x : Float) : String :=
  let absx := if x < 0.0 then -x else x
  if absx < 0.0005 then
    "0.000"
  else
    let y := if x ≥ 0.0 then x + 0.0005 else x - 0.0005
    let s := y.toString
    let parts := s.splitOn "."
    let intPart :=
      match parts with
      | p :: _ => if p == "-0" then "0" else p
      | [] => "0"
    let fracPart :=
      match parts.drop 1 with
      | f :: _ => (f ++ "000").take 3
      | [] => "000"
    intPart ++ "." ++ fracPart

-- solve single block
private def solveBlock (v : Float) (fires : Array Fire) : (Float × Float) :=
  Id.run do
    let minT := fires.foldl (init := fires[0]!.t) fun m f => if f.t < m then f.t else m
    let maxT := fires.foldl (init := fires[0]!.t) fun m f => if f.t > m then f.t else m
    let mut lo := minT
    let mut hi := maxT + 360.0 / v + 10.0
    for _ in [0:80] do
      let mid := (lo + hi) / 2.0
      if covered fires v mid then
        hi := mid
      else
        lo := mid
    let T := hi
    let mut bestX : Float := 0.0
    let mut bestNorm : Float := 0.0
    let mut found : Bool := false
    for f in fires do
      if T ≥ f.t then
        let r := v * (T - f.t)
        let l := norm (f.x - r)
        let rpos := norm (f.x + r)
        for cand in #[l, rpos] do
          let t := arrival fires v cand
          if Float.abs (t - T) < 1e-4 then
            let normc := if cand < 0.0 then cand + 360.0 else cand
            if (!found) || normc < bestNorm then
              found := true
              bestNorm := normc
              bestX := cand
    let coord := if bestX > 180.0 then bestX - 360.0 else bestX
    return (T, coord)

-- process tokens
partial def process (toks : Array String) (idx : Nat) (acc : List String) : IO Unit := do
  if idx < toks.size then
    let v := parseFloat toks[idx]!
    if v == -1.0 then
      for line in acc.reverse do
        IO.println line
    else
      let n := toks[idx+1]!.toNat!
      let mut fires : Array Fire := Array.mkEmpty n
      let mut j := idx + 2
      for _ in [0:n] do
        let t := parseFloat toks[j]!
        let x := parseFloat toks[j+1]!
        fires := fires.push {t := t, x := norm x}
        j := j + 2
      let (time, coord) := solveBlock v fires
      let line := format3 time ++ " " ++ format3 coord
      process toks j (line :: acc)
  else
    for line in acc.reverse do IO.println line

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  process toks 0 []
