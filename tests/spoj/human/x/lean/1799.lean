/- Solution for SPOJ BOTTOM - The Bottom of a Graph
https://www.spoj.com/problems/BOTTOM/
-/

import Std
open Std

/-- Build adjacency lists for the graph and its reverse from the edge list. -/
private def buildAdj (n : Nat) (edges : List Nat) : Array (List Nat) × Array (List Nat) :=
  let rec go (es : List Nat) (adj rev : Array (List Nat)) :=
    match es with
    | u :: v :: rest =>
        let adj := adj.modify u (fun l => v :: l)
        let rev := rev.modify v (fun l => u :: l)
        go rest adj rev
    | _ => (adj, rev)
  go edges (Array.mkArray (n + 1) []) (Array.mkArray (n + 1) [])

/-- First DFS pass collecting vertices by finish time. -/
partial def dfs1 (u : Nat) (adj : Array (List Nat))
    (visRef : IO.Ref (Array Bool)) (orderRef : IO.Ref (List Nat)) : IO Unit := do
  let vis ← visRef.get
  if vis[u]! then
    pure ()
  else
    visRef.modify (fun a => a.set! u true)
    for v in adj[u]! do
      dfs1 v adj visRef orderRef
    orderRef.modify (fun s => u :: s)

/-- Second DFS pass assigning component identifiers. -/
partial def dfs2 (u : Nat) (rev : Array (List Nat))
    (visRef : IO.Ref (Array Bool)) (compRef : IO.Ref (Array Nat)) (cid : Nat) : IO Unit := do
  let vis ← visRef.get
  if vis[u]! then
    pure ()
  else
    visRef.modify (fun a => a.set! u true)
    compRef.modify (fun a => a.set! u cid)
    for v in rev[u]! do
      dfs2 v rev visRef compRef cid

/-- Compute the vertices in the bottom (sinks) of the graph. -/
partial def bottom (n : Nat) (adj rev : Array (List Nat)) : IO String := do
  let visited1 ← IO.mkRef (Array.mkArray (n + 1) false)
  let orderRef ← IO.mkRef ([] : List Nat)
  for i in [1:n+1] do
    let vis ← visited1.get
    if !vis[i]! then
      dfs1 i adj visited1 orderRef
  let order := (← orderRef.get)
  let visited2 ← IO.mkRef (Array.mkArray (n + 1) false)
  let compRef ← IO.mkRef (Array.mkArray (n + 1) 0)
  let mut cid := 0
  for u in order do
    let vis ← visited2.get
    if !vis[u]! then
      cid := cid + 1
      dfs2 u rev visited2 compRef cid
  let compIds := (← compRef.get)
  let mut hasOut := Array.mkArray (cid + 1) false
  for u in [1:n+1] do
    for v in adj[u]! do
      if compIds[u]! ≠ compIds[v]! then
        hasOut := hasOut.set! (compIds[u]!) true
  let mut res : List Nat := []
  for u in [1:n+1] do
    if !hasOut[compIds[u]!] then
      res := u :: res
  let strs := res.reverse.map (·.toString)
  return String.intercalate " " strs

/-- Solve a single test case given number of vertices and edge list tokens. -/
partial def solveCase (n e : Nat) (tokens : List Nat) : IO (String × List Nat) := do
  let needed := 2 * e
  let (edgeNums, rest) := tokens.splitAt needed
  let (adj, rev) := buildAdj n edgeNums
  let out ← bottom n adj rev
  return (out, rest)

/-- Process all test cases from token list until a zero is encountered. -/
partial def processCases (tokens : List Nat) (acc : List String) : IO (List String) := do
  match tokens with
  | [] => pure acc.reverse
  | 0 :: _ => pure acc.reverse
  | n :: rest =>
      match rest with
      | e :: more =>
          let (out, rest') ← solveCase n e more
          processCases rest' (out :: acc)
      | _ => pure acc.reverse

/-- Parse all natural numbers from a string. -/
def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

/-- Main entry point: read input, compute bottom nodes for each test case, print results. -/
def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let ints := parseInts input
  let lines ← processCases ints []
  IO.println (String.intercalate "\n" lines)
