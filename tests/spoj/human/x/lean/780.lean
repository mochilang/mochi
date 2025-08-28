/- Solution for SPOJ ARCHPLG - The Archipelago
https://www.spoj.com/problems/ARCHPLG/
-/
import Std
open Std

structure Terminal where
  name : String
  island : String
  x y : Int
  deriving Repr

structure Graph where
  adj : Array (Array (Nat × Int))
  terminals : Array Terminal

/-- compute Manhattan distance between two terminals -/
def manhattan (a b : Terminal) : Int :=
  Int.natAbs (a.x - b.x) + Int.natAbs (a.y - b.y)

/-- run a simple O(V^2) Dijkstra on graph -/
partial def dijkstra (g : Graph) (start : Nat) : Array Int × Array (Option Nat) := Id.run do
  let n := g.terminals.size
  let inf := (10^15 : Int)
  let mut dist := Array.replicate n inf
  let mut prev : Array (Option Nat) := Array.replicate n none
  let mut used := Array.replicate n false
  dist := dist.set! start 0
  for _ in [0:n] do
    let mut u := n
    let mut best := inf
    for i in [0:n] do
      if !used[i]! && dist[i]! < best then
        u := i
        best := dist[i]!
    if u == n then
      break
    used := used.set! u true
    for (v,w) in g.adj[u]! do
      let alt := dist[u]! + w
      if alt < dist[v]! then
        dist := dist.set! v alt
        prev := prev.set! v (some u)
  return (dist, prev)

partial def readTokens (h : IO.FS.Stream) : IO (List String) := do
  let line ← h.getLine
  pure <| (line.trim.split (· = ' ')).filter (· ≠ "")

/-- build graph from the complex input, ignoring restricted areas -/
partial def readCase (h : IO.FS.Stream) : IO (Graph × Nat × Nat) := do
  let n := (← h.getLine).trim.toNat!
  let mut terminals : Array Terminal := #[]
  let mut idMap : Std.HashMap (String × String) Nat := {}
  for _ in [0:n] do
    let islandName := (← h.getLine).trim
    let dims := (← readTokens h)
    let w := dims[0]!.toNat!
    let hgt := dims[1]!.toNat!
    let b := (← h.getLine).trim.toNat!
    for _ in [0:b] do
      let parts ← readTokens h
      let tname := parts[0]!
      let x := parts[1]!.toInt!
      let y := parts[2]!.toInt!
      let idx := terminals.size
      terminals := terminals.push {name := tname, island := islandName, x, y}
      idMap := idMap.insert (tname, islandName) idx
    let f := (← h.getLine).trim.toNat!
    for _ in [0:f] do
      discard <| h.getLine  -- ignore restricted rectangles
  -- prepare adjacency for terminals
  let mut adj : Array (Array (Nat × Int)) := Array.replicate terminals.size #[]
  -- add walking edges within same island
  for i in [0:terminals.size] do
    for j in [i+1:terminals.size] do
      if terminals[i]!.island == terminals[j]!.island then
        let d := manhattan terminals[i]! terminals[j]!
        adj := adj.set! i (adj[i]! |>.push (j,d))
        adj := adj.set! j (adj[j]! |>.push (i,d))
  let m := (← h.getLine).trim.toNat!
  for _ in [0:m] do
    let parts ← readTokens h
    let a := idMap.find! (parts[0]!, parts[1]!)
    let b := idMap.find! (parts[2]!, parts[3]!)
    let w := parts[4]!.toInt!
    adj := adj.set! a (adj[a]! |>.push (b, w))
    adj := adj.set! b (adj[b]! |>.push (a, w))
  let parts ← readTokens h
  let start := idMap.find! (parts[0]!, parts[1]!)
  let goal := idMap.find! (parts[2]!, parts[3]!)
  return ({adj, terminals}, start, goal)

partial def process (h : IO.FS.Stream) (t : Nat) (caseIdx : Nat := 1) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let (graph, s, g) ← readCase h
    let (dist, prev) := dijkstra graph s
    let mut path : List Nat := []
    let mut cur := g
    while true do
      path := cur :: path
      match prev[cur]! with
      | some p => cur := p
      | none => break
    IO.println s!"case {caseIdx} Y"
    IO.println dist[g]!
    for idx in path do
      let tm := graph.terminals[idx]!
      IO.println s!"{tm.name} {tm.island}"
    IO.println ""
    process h (t - 1) (caseIdx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
