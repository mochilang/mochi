/- Solution for SPOJ PANIC - Panic in the Plazas
https://www.spoj.com/problems/PANIC/
-/

import Std
open Std

-- a big value representing infinity
private def INF : Nat := 1000000000000000000

-- priority queue implemented as sorted list
private def insertPQ (p : Nat × Nat) (pq : List (Nat × Nat)) : List (Nat × Nat) :=
  match pq with
  | [] => [p]
  | q :: qs => if p.fst ≤ q.fst then p :: pq else q :: insertPQ p qs

-- multi source Dijkstra using simple list based PQ (sufficient for small tests)
private def dijkstra (n : Nat) (adj : Array (List (Nat × Nat))) (sources : List Nat) : Array Nat :=
  Id.run <| do
    let mut dist := Array.mkArray (n+1) INF
    let mut pq : List (Nat × Nat) := []
    for s in sources do
      dist := dist.set! s 0
      pq := insertPQ (0, s) pq
    let rec loop (dist : Array Nat) (pq : List (Nat × Nat)) : Array Nat :=
      match pq with
      | [] => dist
      | (d,v) :: qs =>
          if d > dist.get! v then
            loop dist qs
          else
            let mut dist := dist
            let mut pq := qs
            for (to,w) in adj.get! v do
              let nd := d + w
              if nd < dist.get! to then
                dist := dist.set! to nd
                pq := insertPQ (nd, to) pq
            loop dist pq
    loop dist pq

-- compare two rational numbers a/b and c/d
private def ratCmp (a b c d : Nat) : Ordering :=
  let lhs := a * d
  let rhs := c * b
  if lhs < rhs then Ordering.lt else if lhs = rhs then Ordering.eq else Ordering.gt

-- depth first search along predecessor edges
private partial def dfs (v : Nat) (pred : Array (List Nat)) (vis : Array Bool) : Array Bool :=
  if vis.get! v then vis
  else
    let vis := vis.set! v true
    pred.get! v |>.foldl (fun acc u => dfs u pred acc) vis

-- solve a single test case
private def solveCase (it : List Nat) : (List Nat × List Nat) :=
  -- returns (remaining tokens, answer list)
  let n := it.head!
  let m := it.tail!.head!
  let k := it.tail!.tail!.head!
  -- parse edges
  let mut rest := it.drop 3
  let mut edges : List (Nat × Nat × Nat × Nat) := []
  let mut adj := Array.mkArray (n+1) (List.empty : List (Nat × Nat))
  for _ in List.range m do
    let u := rest.head!
    let v := rest.tail!.head!
    let uv := rest.tail!.tail!.head!
    let vu := rest.tail!.tail!.tail!.head!
    rest := rest.drop 4
    edges := (u,v,uv,vu) :: edges
    adj := adj.set! u ((v,uv) :: adj.get! u)
    adj := adj.set! v ((u,vu) :: adj.get! v)
  -- parse sources
  let mut sources : List Nat := []
  for _ in List.range k do
    let s := rest.head!
    rest := rest.tail!
    sources := s :: sources
  let dist := dijkstra n adj sources
  -- collect unreachable nodes
  let unreachable :=
    (List.range n).foldl
      (fun acc i =>
        let idx := i+1
        if dist.get! idx = INF then acc.push idx else acc)
      ([] : List Nat)
  if unreachable ≠ [] then
    (rest, unreachable)
  else
    -- compute max time among nodes and edges
    let mut maxNum := 0
    let mut maxDen := 1
    -- nodes
    for i in List.range n do
      let t := dist.get! (i+1)
      if ratCmp (t) 1 maxNum maxDen = Ordering.gt then
        maxNum := t
        maxDen := 1
    -- edges
    let mut bestEdges : List (Nat × Nat) := []
    for (u,v,uv,vu) in edges do
      let num := uv*vu + dist.get! u * vu + dist.get! v * uv
      let den := uv + vu
      match ratCmp num den maxNum maxDen with
      | Ordering.gt =>
          maxNum := num
          maxDen := den
          bestEdges := [(u,v)]
      | Ordering.eq =>
          bestEdges := (u,v) :: bestEdges
      | Ordering.lt => pure ()
    -- build predecessor graph
    let mut pred := Array.mkArray (n+1) ([] : List Nat)
    for (u,v,uv,vu) in edges do
      if dist.get! u + uv = dist.get! v then
        pred := pred.set! v (u :: pred.get! v)
      if dist.get! v + vu = dist.get! u then
        pred := pred.set! u (v :: pred.get! u)
    -- initial nodes: nodes with dist == maxTime and endpoints of best edges
    let mut init : List Nat := []
    for i in List.range n do
      let idx := i+1
      if dist.get! idx * maxDen = maxNum then
        init := idx :: init
    for (u,v) in bestEdges do
      init := u :: init
      init := v :: init
    -- dfs to collect all ancestors
    let mut vis := Array.mkArray (n+1) false
    for v in init do
      vis := dfs v pred vis
    let ans :=
      (List.range n).foldl (fun acc i =>
        let idx := i+1
        if vis.get! idx then acc.push idx else acc) []
    (rest, ans)

-- parse all tokens and solve
partial def solveAll (it : List Nat) (t : Nat) (acc : List (List Nat)) : List (List Nat) :=
  if t = 0 then acc.reverse
  else
    let (rest, ans) := solveCase it
    solveAll rest (t-1) (ans :: acc)

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let nums := input.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.map String.toNat!
  let t := nums.head!
  let results := solveAll nums.tail! t []
  for res in results do
    let line := String.intercalate " " (res.map toString)
    IO.println line
