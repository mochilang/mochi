/- Solution for SPOJ SEGVIS - Horizontally Visible Segments
https://www.spoj.com/problems/SEGVIS/
-/

import Std
open Std

structure Seg where
  y1 : Nat
  y2 : Nat
  x  : Nat
  deriving Repr

def insertByX (active : List Nat) (i : Nat) (segs : Array Seg) : List Nat :=
  match active with
  | [] => [i]
  | j :: rest =>
    if (segs.get! i).x <= (segs.get! j).x then
      i :: active
    else
      j :: insertByX rest i segs

def removeIdx (active : List Nat) (i : Nat) : List Nat :=
  active.filter (fun j => j ≠ i)

def addEdges (active : List Nat) (edges : Std.HashSet (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
  let rec loop : List Nat → Std.HashSet (Nat × Nat) → Std.HashSet (Nat × Nat)
  | i :: j :: rest, e =>
      let e := e.insert (if i < j then (i, j) else (j, i))
      loop (j :: rest) e
  | _, e => e
  loop active edges

def countTriangles (segs : Array Seg) : Nat :=
  let maxY := 8000
  let (start, stop) :=
    (List.range segs.size).foldl
      (fun (s : Array (List Nat) × Array (List Nat)) i =>
        let seg := segs.get! i
        let s1 := s.fst.modify seg.y1 (fun l => i :: l)
        let s2 := s.snd.modify seg.y2 (fun l => i :: l)
        (s1, s2))
      (Array.mkArray (maxY+1) [], Array.mkArray (maxY+1) [])
  let edges := Id.run <|
    let mut edges : Std.HashSet (Nat × Nat) := {}
    let mut active : List Nat := []
    for y in [0:maxY+1] do
      for idx in start.get! y do
        active := insertByX active idx segs
      edges := addEdges active edges
      for idx in stop.get! y do
        active := removeIdx active idx
      if y < maxY then
        edges := addEdges active edges
    edges
  let mut adj : Array (Std.HashSet Nat) := Array.mkArray segs.size {}
  for (i, j) in edges.toList do
    adj := adj.modify i (fun s => s.insert j)
    adj := adj.modify j (fun s => s.insert i)
  let mut count := 0
  for i in [0:segs.size] do
    for j in (adj.get! i).toList do
      if j > i then
        for k in (adj.get! j).toList do
          if k > j ∧ (adj.get! i).contains k then
            count := count + 1
  count

def parseNat (s : String) : Nat := s.trim.toNat!

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let toks := input.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t').filter (· ≠ "")
  let mut i := 0
  let next : IO Nat := do
    let t := toks.get! i
    i := i + 1
    pure (parseNat t)
  let d ← next
  for _ in [0:d] do
    let n ← next
    let mut segs : Array Seg := #[]
    for _ in [0:n] do
      let y1 ← next; let y2 ← next; let x ← next
      segs := segs.push {y1 := y1, y2 := y2, x := x}
    IO.println (countTriangles segs)
