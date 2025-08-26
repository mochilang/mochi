/- Solution for SPOJ LABYR1 - Labyrinth
https://www.spoj.com/problems/LABYR1/
-/
import Std
open Std

abbrev Grid := Array (Array Char)

def neighbors (g : Grid) (r c : Nat) : List (Nat × Nat) :=
  let rows := g.size
  let cols := (g.get! 0).size
  let res : List (Nat × Nat) := []
  let res := if r > 0 && (g.get! (r-1)).get! c = '.' then (r-1,c) :: res else res
  let res := if r + 1 < rows && (g.get! (r+1)).get! c = '.' then (r+1,c) :: res else res
  let res := if c > 0 && (g.get! r).get! (c-1) = '.' then (r,c-1) :: res else res
  let res := if c + 1 < cols && (g.get! r).get! (c+1) = '.' then (r,c+1) :: res else res
  res

partial def bfs (g : Grid) (start : Nat × Nat) : (Nat × Nat × Nat) :=
  let cols := (g.get! 0).size
  let encode (p : Nat × Nat) : Nat := p.fst * cols + p.snd
  let rec loop (q : List ((Nat × Nat) × Nat)) (vis : Std.HashSet Nat)
      (best : Nat × Nat × Nat) : (Nat × Nat × Nat) :=
    match q with
    | [] => best
    | ((p@(r,c)), d) :: qs =>
        let id := encode p
        if vis.contains id then
          loop qs vis best
        else
          let vis := vis.insert id
          let best := if d > best.2 then (r, c, d) else best
          let neigh := neighbors g r c
          let neigh := neigh.filter (fun xy => !vis.contains (encode xy))
          let newQ := qs ++ neigh.map (fun xy => (xy, d+1))
          loop newQ vis best
  loop [((start), 0)] (Std.HashSet.empty) (start.1, start.2, 0)

def findStart (g : Grid) : Option (Nat × Nat) :=
  Id.run do
    let mut ans : Option (Nat × Nat) := none
    for r in [0:g.size] do
      if ans.isNone then
        let row := g.get! r
        for c in [0:row.size] do
          if row.get! c = '.' && ans.isNone then
            ans := some (r, c)
    pure ans

def maxRope (g : Grid) : Nat :=
  match findStart g with
  | none => 0
  | some s =>
      let (r1, c1, _) := bfs g s
      let (_, _, d) := bfs g (r1, c1)
      d

partial def readGrid (h : IO.FS.Stream) (r : Nat) : IO Grid := do
  let mut arr : Grid := Array.mkEmpty r
  for _ in [0:r] do
    let line := (← h.getLine).trim
    arr := arr.push (line.toList.toArray)
  pure arr

partial def readPair (h : IO.FS.Stream) : IO (Nat × Nat) := do
  let rec loop : IO (Nat × Nat) := do
    let line ← h.getLine
    let t := line.trim
    if t.isEmpty then loop else
      let parts := t.splitOn " "
      pure (parts.get! 0 |>.toNat!, parts.get! 1 |>.toNat!)
  loop

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let (c, r) ← readPair h
    let grid ← readGrid h r
    let res := maxRope grid
    IO.println s!"Maximum rope length is {res}."
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
