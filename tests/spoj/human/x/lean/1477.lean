/- Solution for SPOJ PT07A - Play with a Tree
https://www.spoj.com/problems/PT07A/
-/

import Std
open Std

partial def prune (adj : Array (Array Nat)) (ground : Array Bool)
    (q : List Nat) (deg : Array Nat) (removed : Array Bool)
    : Array Nat × Array Bool :=
  match q with
  | [] => (deg, removed)
  | u :: qs =>
    if removed.get! u then
      prune adj ground qs deg removed
    else
      let removed := removed.set! u true
      let (deg, qs) := (adj.get! u).foldl
        (fun (dr : Array Nat × List Nat) v =>
          let (deg, qs) := dr
          if removed.get! v then (deg, qs)
          else
            let d := (deg.get! v) - 1
            let deg := deg.set! v d
            let qs := if d = 1 && !ground.get! v then v :: qs else qs
            (deg, qs))
        (deg, qs)
      prune adj ground qs deg removed

partial def dfs (adj : Array (Array Nat)) (skeleton : Array Bool)
    (u parent : Nat) (visited : Array Bool) : Nat × Array Bool :=
  let visited := visited.set! u true
  let (sg, visited) := (adj.get! u).foldl
    (fun (sv : Nat × Array Bool) v =>
      let (sg, visited) := sv
      if v = parent || skeleton.get! v || visited.get! v then (sg, visited)
      else
        let (child, visited) := dfs adj skeleton v u visited
        (Nat.xor sg (child + 1), visited))
    (0, visited)
  (sg, visited)

partial def solveCase (n : Nat) (ground : Array Bool) (adj : Array (Array Nat)) : Nat :=
  Id.run do
    let mut deg := Array.mkArray n 0
    for i in [0:n] do
      deg := deg.set! i ((adj.get! i).size)
    let mut removed := Array.mkArray n false
    let mut q : List Nat := []
    for i in [0:n] do
      if deg.get! i = 1 && !ground.get! i then
        q := i :: q
    let (deg, removed) := prune adj ground q deg removed
    let skeleton := removed.map fun r => !r
    let mut edges := 0
    for i in [0:n] do
      if skeleton.get! i then
        for v in adj.get! i do
          if v > i && skeleton.get! v then
            edges := edges + 1
    let mut gval := edges % 2
    let mut visited := Array.mkArray n false
    for i in [0:n] do
      if skeleton.get! i then
        for v in adj.get! i do
          if !skeleton.get! v && !visited.get! v then
            let (sg, vis') := dfs adj skeleton v i visited
            visited := vis'
            gval := Nat.xor gval (sg + 1)
    return gval

partial def loop (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := (toks.get! idx).toNat!
    let mut ground := Array.mkArray n false
    let mut j := idx + 1
    for i in [0:n] do
      ground := ground.set! i ((toks.get! j).toNat! = 1)
      j := j + 1
    let mut adj : Array (Array Nat) := Array.mkArray n #[]
    for _ in [0:n-1] do
      let u := (toks.get! j).toNat! - 1
      let v := (toks.get! (j+1)).toNat! - 1
      adj := adj.set! u ((adj.get! u).push v)
      adj := adj.set! v ((adj.get! v).push u)
      j := j + 2
    let g := solveCase n ground adj
    let acc := (if g ≠ 0 then "1" else "0") :: acc
    loop toks j (t-1) acc

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := (toks.get! 0).toNat!
  let outs := loop toks 1 t []
  IO.println (String.intercalate "\n" outs)
