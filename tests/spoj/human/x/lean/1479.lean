/- Solution for SPOJ PT07C - The GbAaY Kingdom
https://www.spoj.com/problems/PT07C/
-/
import Std
open Std

/-- Split input string into natural numbers --/
def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

/-- Consume m triples from a list of numbers as edges --/
partial def parseEdges : Nat -> List Nat -> List (Nat × Nat × Nat)
  -> (List (Nat × Nat × Nat) × List Nat)
| 0, xs, acc => (acc.reverse, xs)
| Nat.succ m, u :: v :: w :: rest, acc =>
    parseEdges m rest ((u, v, w) :: acc)
| _, xs, acc => (acc.reverse, xs)

/-- Main solver returning diameter and list of edges --/
def solve (n : Nat) (edges : List (Nat × Nat × Nat)) : (Nat × List (Nat × Nat)) :=
  Id.run do
    let inf := 1000000000
    let size := n + 1
    -- distance matrix
    let mut dist := Array.replicate size (Array.replicate size inf)
    for i in [1:size] do
      dist := dist.set! i ((dist[i]!).set! i 0)
    -- adjacency list
    let mut adj : Array (List (Nat × Nat)) := Array.replicate size []
    for (u,v,w) in edges do
      let rowU := dist[u]!
      let rowU := rowU.set! v (Nat.min (rowU[v]! ) w)
      dist := dist.set! u rowU
      let rowV := dist[v]!
      let rowV := rowV.set! u (Nat.min (rowV[u]! ) w)
      dist := dist.set! v rowV
      adj := adj.set! u ((v,w) :: adj[u]!)
      adj := adj.set! v ((u,w) :: adj[v]!)
    -- Floyd-Warshall
    for k in [1:size] do
      for i in [1:size] do
        let dik := dist[i]![k]!
        if dik < inf then
          for j in [1:size] do
            let new := dik + dist[k]![j]!
            if new < dist[i]![j]! then
              let row := dist[i]!
              dist := dist.set! i (row.set! j new)
    -- search best center
    let mut bestD := 2 * inf
    let mut best : Option (Sum Nat (Nat × Nat × Nat × Nat)) := none
    -- vertex centers
    for v in [1:size] do
      let ecc := (dist[v]!).foldl (fun m x => Nat.max m x) 0
      let cand := 2 * ecc
      if cand < bestD then
        bestD := cand
        best := some (Sum.inl v)
    -- edge centers
    for u in [1:size] do
      for (v,w) in adj[u]! do
        if u < v then
          let mut maxVal := 0
          let mut arg := 1
          for i in [1:size] do
            let s := dist[u]![i]! + dist[v]![i]!
            if s > maxVal then
              maxVal := s
              arg := i
          let cand := maxVal + w
          if cand < bestD then
            bestD := cand
            best := some (Sum.inr (u,v,w,arg))
    -- construct tree
    let ans := match best with
    | some (Sum.inl root) =>
        let mut res : Array (Nat × Nat) := #[]
        for x in [1:size] do
          if x != root then
            let mut parent := 0
            for (y,w) in adj[x]! do
              if (dist[root]![x]! == dist[root]![y]! + w) && (dist[root]![y]! < dist[root]![x]!) then
                parent := y
            if parent > 0 then
              res := res.push (if x < parent then (x,parent) else (parent,x))
        (bestD, res.toList)
    | some (Sum.inr (u,v,w,arg)) =>
        let mut res : Array (Nat × Nat) := #[]
        let delta : Int := Int.ofNat (dist[u]![arg]!) - Int.ofNat (dist[v]![arg]!)
        for x in [1:size] do
          if x != u && x != v then
            let du := dist[u]![x]!
            let dv := dist[v]![x]!
            let useU := (Int.ofNat du - Int.ofNat dv) ≤ delta
            let root := if useU then u else v
            let dval := if useU then du else dv
            let mut parent := 0
            for (y,w2) in adj[x]! do
              if (dval == dist[root]![y]! + w2) && (dist[root]![y]! < dval) then
                parent := y
            if parent > 0 then
              res := res.push (if x < parent then (x,parent) else (parent,x))
        res := res.push (if u < v then (u,v) else (v,u))
        (bestD, res.toList)
    | none => (0, [])
    return ans

/-- Entry point --/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let ints := parseInts input
  match ints with
  | n :: m :: rest =>
      let (edges, _) := parseEdges m rest []
      let (d, es) := solve n edges
      let lines := (toString d) :: es.map (fun (a,b) => s!"{a} {b}")
      IO.println (String.intercalate "\n" lines)
  | _ => pure ()
