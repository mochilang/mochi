/- Solution for SPOJ OPTSUB - Optimal Connected Subset
https://www.spoj.com/problems/OPTSUB/
-/

import Std
open Std

partial def dfs (u p : Nat) (adj : Array (Array Nat)) (w : Array Int) (best : IO.Ref Int) : IO Int := do
  let row := adj[u]!
  let mut total := w[u]!
  for v in row do
    if v ≠ p then
      let child ← dfs v u adj w best
      if child > 0 then
        total := total + child
  let b ← best.get
  if total > b then best.set total
  pure total

partial def solveCase (n : Nat) (coords : Array (Int × Int)) (w : Array Int) : IO Int := do
  let mut adj : Array (Array Nat) := Array.replicate n #[]
  for i in [0:n] do
    for j in [i+1:n] do
      let (x1, y1) := coords[i]!
      let (x2, y2) := coords[j]!
      let dist := Int.natAbs (x1 - x2) + Int.natAbs (y1 - y2)
      if dist == 1 then
        adj := adj.modify i (fun arr => arr.push j)
        adj := adj.modify j (fun arr => arr.push i)
  let best ← IO.mkRef (-1000000000 : Int)
  _ ← dfs 0 n adj w best
  best.get

partial def process (toks : Array String) (idxRef : IO.Ref Nat) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let idx ← idxRef.get
    let n := toks[idx]!.toNat!
    idxRef.set (idx + 1)
    let mut coords : Array (Int × Int) := #[]
    let mut w : Array Int := #[]
    for _ in [0:n] do
      let idx ← idxRef.get
      let x := toks[idx]!.toInt!
      let y := toks[idx+1]!.toInt!
      let c := toks[idx+2]!.toInt!
      idxRef.set (idx + 3)
      coords := coords.push (x,y)
      w := w.push c
    let ans ← solveCase n coords w
    IO.println ans
    process toks idxRef (t-1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toksList := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let toks := toksList.toArray
  let t := toks[0]!.toNat!
  let idxRef ← IO.mkRef 1
  process toks idxRef t
