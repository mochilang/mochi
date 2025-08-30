/- Solution for SPOJ PT07G - Colorful Lights Party
https://www.spoj.com/problems/PT07G/
-/
import Std
open Std

/-- graceful labeling for small hall tree using backtracking over edge differences --/
partial def gracefulSmall (n : Nat) (edges : List (Nat × Nat)) : List Int := Id.run do
  -- adjacency list
  let mut adj : Array (List Nat) := Array.replicate (n+1) []
  for (u,v) in edges do
    adj := adj.modify u (fun l => v :: l)
    adj := adj.modify v (fun l => u :: l)
  -- labels array, n means unlabeled
  let mut labels := Array.replicate (n+1) n
  let mut used := Array.replicate n false
  labels := labels.set! 1 0
  used := used.set! 0 true
  let rec dfs (d : Nat) (labels : Array Nat) (used : Array Bool) (labeled : List Nat) : Option (Array Nat) :=
    if d == 0 then
      some labels
    else
      let rec tryNodes : List Nat → Option (Array Nat)
        | [] => none
        | u :: us =>
            let lu := labels[u]!
            let rec tryNeigh : List Nat → Option (Array Nat)
              | [] => tryNodes us
              | v :: vs =>
                  if labels[v]! != n then
                    tryNeigh vs
                  else
                    let diff := d
                    let c1 := lu + diff
                    let mut res := none
                    if c1 < n ∧ ¬ used[c1]! then
                      let labels' := labels.set! v c1
                      let used' := used.set! c1 true
                      res := dfs (d-1) labels' used' (v :: labeled)
                    let res := match res with
                                | some _ => res
                                | none =>
                                    if lu ≥ diff then
                                      let c2 := lu - diff
                                      if c2 < n ∧ ¬ used[c2]! then
                                        let labels' := labels.set! v c2
                                        let used' := used.set! c2 true
                                        dfs (d-1) labels' used' (v :: labeled)
                                      else
                                        tryNeigh vs
                                    else
                                      tryNeigh vs
                    match res with
                    | some _ => res
                    | none => tryNeigh vs
            tryNeigh (adj[u]!)
      tryNodes labeled
  match dfs (n-1) labels used [1] with
  | some arr =>
      (List.range n).map (fun i => Int.ofNat (arr[i+1]!))
  | none => List.replicate n (-1)

/-- graceful labeling for large hall spider structure --/
def gracefulLarge (k t : Nat) : List Int :=
  let m := t + 1
  let n := 1 + k * m
  let mut arr : Array Int := Array.replicate n 0
  arr := arr.set! 0 0
  for i in [1:k+1] do
    for j in [1:m+1] do
      let label := if j % 2 == 1
        then Int.ofNat ((k - i + 1) * m - (j-1)/2)
        else Int.ofNat ((i - 1) * m + j/2)
      let id := 1 + (j-1) * k + i
      arr := arr.set! id label
  (List.range n).map (fun i => arr[i]!)

partial def parseEdges (toks : Array String) (idx cnt : Nat) (acc : List (Nat × Nat)) : (List (Nat × Nat) × Nat) :=
  if cnt == 0 then (acc.reverse, idx) else
    let u := (toks[idx]!).toNat!
    let v := (toks[idx+1]!).toNat!
    parseEdges toks (idx+2) (cnt-1) ((u,v) :: acc)

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t == 0 then acc.reverse else
    let kind := (toks[idx]!).toNat!
    if kind == 1 then
      let n := (toks[idx+1]!).toNat!
      let (edges, idx') := parseEdges toks (idx+2) (n-1) []
      let labels := gracefulSmall n edges
      let line := String.intercalate " " (labels.map toString)
      solveAll toks idx' (t-1) (line :: acc)
    else
      let k := (toks[idx+1]!).toNat!
      let tt := (toks[idx+2]!).toNat!
      let labels := gracefulLarge k tt
      let line := String.intercalate " " (labels.map toString)
      solveAll toks (idx+3) (t-1) (line :: acc)

/-- read entire stdin --/
def readStdin : IO String := do
  let h ← IO.getStdin
  h.readToEnd

def main : IO Unit := do
  let data ← readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := (toks[0]!).toNat!
    let outs := solveAll toks 1 t []
    for line in outs do
      IO.println line
