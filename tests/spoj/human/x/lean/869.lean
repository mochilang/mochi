/- Solution for SPOJ IMPORT - Galactic Import
https://www.spoj.com/problems/IMPORT/
-/

import Std
open Std

/-- Convert letter A-Z to index 0-25. -/
private def idxOf (c : Char) : Nat := c.toNat - 'A'.toNat

/-- Parse a decimal string like d.dd into Float. -/
private def parseFloat (s : String) : Float :=
  let parts := s.splitOn "."
  match parts with
  | [a, b] =>
      let intPart := a.toNat!
      let fracPart := b.toNat!
      let denom := Float.ofNat (10 ^ b.length)
      Float.ofNat intPart + Float.ofNat fracPart / denom
  | _ => 0.0

/-- Compute (0.95)^n. -/
private def pow095 (n : Nat) : Float :=
  let rec loop (k : Nat) (acc : Float) : Float :=
    match k with
    | 0 => acc
    | Nat.succ k' => loop k' (acc * 0.95)
  loop n 1.0

/-- Solve one galaxy description. -/
private def solveGalaxy (planets : Array (Char × Float × String)) : String := Id.run do
  let mut adj : Array (List Nat) := Array.replicate 26 []
  let mut val : Array Float := Array.replicate 26 0.0
  let mut star : Array Bool := Array.replicate 26 false
  let mut present : Array Bool := Array.replicate 26 false
  for (name, v, conn) in planets do
    let i := idxOf name
    val := val.set! i v
    present := present.set! i true
    let mut hasStar := false
    for c in conn.data do
      if c = '*' then
        hasStar := true
      else
        let j := idxOf c
        adj := adj.modify i (fun l => j :: l)
        adj := adj.modify j (fun l => i :: l)
    star := star.set! i hasStar
  -- BFS from all star planets
  let mut dist : Array Nat := Array.replicate 26 1000
  let mut queue : Array Nat := #[]
  for i in [0:26] do
    if star[i]! then
      dist := dist.set! i 0
      queue := queue.push i
  let mut head : Nat := 0
  while head < queue.size do
    let u := queue[head]!
    head := head + 1
    let du := dist[u]!
    for v in adj[u]! do
      if dist[v]! == 1000 then
        dist := dist.set! v (du + 1)
        queue := queue.push v
  -- choose best planet
  let mut bestVal : Float := -1.0
  let mut bestIdx : Nat := 0
  for i in [0:26] do
    if present[i]! then
      let d := dist[i]!
      if d ≠ 1000 then
        let eff := val[i]! * pow095 d
        if eff > bestVal || (eff == bestVal && i < bestIdx) then
          bestVal := eff
          bestIdx := i
  let ch := Char.ofNat (bestIdx + 'A'.toNat)
  return s!"Import from {ch}"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  if data.trim.isEmpty then
    pure ()
  else
    let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
    let arr := List.toArray toks
    let mut idx : Nat := 0
    let mut out : Array String := #[]
    while idx < arr.size do
      let n := arr[idx]!.toNat!
      idx := idx + 1
      let mut nodes : Array (Char × Float × String) := #[]
      for _ in [0:n] do
        let ch := arr[idx]!.get! 0
        let v := parseFloat (arr[idx+1]!)
        let s := arr[idx+2]!
        nodes := nodes.push (ch, v, s)
        idx := idx + 3
      out := out.push (solveGalaxy nodes)
    IO.println (String.intercalate "\n" out.toList)
