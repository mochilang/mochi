/- Solution for SPOJ BIA - Bytelandian Information Agency
https://www.spoj.com/problems/BIA/
-/
import Std
open Std

partial def bfsSkip (adj : Array (Array Nat)) (skip : Nat) : Array Bool :=
  let n := adj.size
  let rec loop (queue : List Nat) (vis : Array Bool) : Array Bool :=
    match queue with
    | [] => vis
    | v :: qs =>
      if v = skip || vis.get! v then
        loop qs vis
      else
        let vis := vis.set! v true
        let neigh := (adj.get! v).toList.filter (fun w => w ≠ skip)
        loop (qs ++ neigh) vis
  loop [0] (Array.mkArray n false)

partial def criticalNodes (adj : Array (Array Nat)) : Array Nat :=
  let n := adj.size
  let rec loop (i : Nat) (acc : Array Nat) : Array Nat :=
    if h : i < n then
      if i = 0 then
        loop (i+1) (acc.push (i+1))
      else
        let vis := bfsSkip adj i
        let unreach := (List.range n).any (fun j => j ≠ i && vis.get! j = false)
        let acc := if unreach then acc.push (i+1) else acc
        loop (i+1) acc
    else acc
  loop 0 #[]

partial def readEdges (toks : Array String) (idx m : Nat)
    (adj : Array (Array Nat)) : (Array (Array Nat) × Nat) :=
  if m = 0 then (adj, idx) else
    let a := toks.get! idx |>.toNat! - 1
    let b := toks.get! (idx+1) |>.toNat! - 1
    let adj := adj.modify a (fun arr => arr.push b)
    readEdges toks (idx+2) (m-1) adj

partial def readCases (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  if idx + 1 < toks.size then
    let n := toks.get! idx |>.toNat!
    let m := toks.get! (idx+1) |>.toNat!
    let (adj, nxt) := readEdges toks (idx+2) m (Array.mkArray n #[])
    let crit := criticalNodes adj
    let line1 := toString crit.size
    let line2 := String.intercalate " " (crit.toList.map toString)
    readCases toks nxt (line2 :: line1 :: acc)
  else
    acc.reverse

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let outs := readCases toks 0 []
  for line in outs do
    IO.println line
