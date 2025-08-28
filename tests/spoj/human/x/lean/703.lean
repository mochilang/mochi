/- Solution for SPOJ SERVICE - Mobile Service
https://www.spoj.com/problems/SERVICE/
-/

import Std
open Std

namespace SPOJ703

private def INF : Nat := 1000000000

private def orderPair (a b : Nat) : Nat × Nat :=
  if a ≤ b then (a,b) else (b,a)

private def updateMin (m : Std.HashMap (Nat×Nat) Nat) (k : Nat×Nat) (v : Nat)
    : Std.HashMap (Nat×Nat) Nat :=
  let old := m.findD k INF
  if v < old then m.insert k v else m

private def step (cost : Array (Array Nat)) (prev req : Nat)
    (dp : Std.HashMap (Nat×Nat) Nat) : Std.HashMap (Nat×Nat) Nat :=
  dp.toList.foldl
    (fun acc (kv : (Nat×Nat) × Nat) =>
      let (p, v) := kv
      let a := p.fst
      let b := p.snd
      if req == prev then
        updateMin acc (orderPair a b) v
      else if req == a then
        updateMin acc (orderPair prev b) v
      else if req == b then
        updateMin acc (orderPair prev a) v
      else
        let c1 := v + cost.get! (prev-1) |>.get! (req-1)
        let c2 := v + cost.get! (a-1) |>.get! (req-1)
        let c3 := v + cost.get! (b-1) |>.get! (req-1)
        let acc := updateMin acc (orderPair a b) c1
        let acc := updateMin acc (orderPair prev b) c2
        let acc := updateMin acc (orderPair prev a) c3
        acc
    ) (Std.HashMap.empty)

private def solveCase (cost : Array (Array Nat)) (reqs : Array Nat) : Nat :=
  let n := reqs.size
  let r0 := reqs[0]!
  let mut dp : Std.HashMap (Nat×Nat) Nat := Std.HashMap.empty
  if r0 == 1 then
    dp := updateMin dp (orderPair 2 3) 0
  if r0 == 2 then
    dp := updateMin dp (orderPair 1 3) 0
  if r0 == 3 then
    dp := updateMin dp (orderPair 1 2) 0
  if r0 ≠ 1 && r0 ≠ 2 && r0 ≠ 3 then
    dp := updateMin dp (orderPair 2 3) (cost.get! 0 |>.get! (r0-1))
    dp := updateMin dp (orderPair 1 3) (cost.get! 1 |>.get! (r0-1))
    dp := updateMin dp (orderPair 1 2) (cost.get! 2 |>.get! (r0-1))
  let rec loop (i : Nat) (prev : Nat) (dp : Std.HashMap (Nat×Nat) Nat)
      : Std.HashMap (Nat×Nat) Nat :=
    if h : i < n then
      let req := reqs.get! i
      let dp' := step cost prev req dp
      loop (i+1) req dp'
    else
      dp
  let final := loop 1 r0 dp
  final.toList.foldl (fun acc (_kv : (Nat×Nat) × Nat) =>
      let v := _kv.snd
      if v < acc then v else acc) INF

partial def solve (toks : Array String) : Array String := Id.run do
  let t := toks[0]!.toNat!
  let mut idx := 1
  let mut res : Array String := #[]
  for _ in [0:t] do
    let L := toks[idx]!.toNat!; idx := idx + 1
    let N := toks[idx]!.toNat!; idx := idx + 1
    let mut cost : Array (Array Nat) := #[]
    for _ in [0:L] do
      let mut row : Array Nat := #[]
      for _ in [0:L] do
        row := row.push (toks[idx]!.toNat!)
        idx := idx + 1
      cost := cost.push row
    let mut reqs : Array Nat := #[]
    for _ in [0:N] do
      reqs := reqs.push (toks[idx]!.toNat!)
      idx := idx + 1
    res := res.push (toString (solveCase cost reqs))
  return res

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let out := solve toks
  for line in out do
    IO.println line

end SPOJ703
