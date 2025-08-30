/- Solution for SPOJ PT07F - A Short Vacation in Disneyland
https://www.spoj.com/problems/PT07F/
-/

import Std
open Std

-- compute minimal path cover of a tree and output the paths

private def solveCase (n : Nat) (adj : Array (Array Nat)) : List (List Nat) := Id.run do
  -- first, compute parent and order using DFS stack
  let mut parent : Array Nat := Array.replicate (n + 1) 0
  let mut order : Array Nat := #[]
  let mut stack : List (Nat × Nat) := [(1,0)]
  while stack ≠ [] do
    match stack with
    | (v,p) :: rest =>
      stack := rest
      parent := parent.set! v p
      order := order.push v
      for nb in adj[v]! do
        if nb ≠ p then
          stack := (nb,v) :: stack
    | [] => ()
  -- DP arrays
  let mut dp0 : Array Int := Array.replicate (n + 1) 0
  let mut dp1 : Array Int := Array.replicate (n + 1) 0
  let mut i := order.size
  while i > 0 do
    i := i - 1
    let v := order[i]!
    let mut base : Int := 0
    let mut gains : List (Int × Nat) := []
    for nb in adj[v]! do
      if nb ≠ parent[v]! then
        base := base + dp0[nb]!
        let g : Int := 1 + dp1[nb]! - dp0[nb]!
        gains := (g, nb) :: gains
    let gains := gains.qsort (fun a b => a.fst > b.fst)
    -- dp0: choose up to two children
    let mut val0 := base
    let mut cnt := 0
    for (g, _) in gains do
      if cnt < 2 && g > 0 then
        val0 := val0 + g
        cnt := cnt + 1
    dp0 := dp0.set! v val0
    -- dp1: choose up to one child
    let mut val1 := base
    cnt := 0
    for (g, _) in gains do
      if cnt < 1 && g > 0 then
        val1 := val1 + g
        cnt := cnt + 1
    dp1 := dp1.set! v val1
  -- second pass to reconstruct selected edges
  let mut selected : Array (Array Nat) := Array.replicate (n + 1) #[]
  let mut stack2 : List (Nat × Bool × Nat) := [(1,false,0)]
  while stack2 ≠ [] do
    match stack2 with
    | (u,connected,p) :: rest =>
      stack2 := rest
      let mut gains : List (Int × Nat) := []
      for nb in adj[u]! do
        if nb ≠ p then
          let g : Int := 1 + dp1[nb]! - dp0[nb]!
          gains := (g, nb) :: gains
      let gains := gains.qsort (fun a b => a.fst > b.fst)
      let limit := if connected then 1 else 2
      let mut chosen : List Nat := []
      let mut cnt := 0
      for (g,v) in gains do
        if cnt < limit && g > 0 then
          chosen := v :: chosen
          cnt := cnt + 1
      for nb in adj[u]! do
        if nb ≠ p then
          if chosen.contains nb then
            selected := selected.set! u (selected[u]! |>.push nb)
            selected := selected.set! nb (selected[nb]! |>.push u)
            stack2 := (nb, true, u) :: stack2
          else
            stack2 := (nb, false, u) :: stack2
    | [] => ()
  -- build paths from selected edges
  let mut visited : Array Bool := Array.replicate (n + 1) false
  let mut paths : List (List Nat) := []
  for i in [1:n+1] do
    if visited[i]! = false && selected[i]!.size ≤ 1 then
      let mut cur := i
      let mut prev := 0
      let mut arr : Array Nat := #[]
      while true do
        arr := arr.push cur
        visited := visited.set! cur true
        let nexts := (selected[cur]!).filter (fun v => v ≠ prev)
        if nexts.size = 0 then
          break
        else
          prev := cur
          cur := nexts[0]!
      paths := arr.toList :: paths
  return paths.reverse

-- solve entire input

private def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array String := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p
  pure arr

def main : IO Unit := do
  let tokens ← readTokens
  let mut idx : Nat := 0
  let t := tokens[idx]!.toNat!; idx := idx + 1
  for _ in [0:t] do
    let n := tokens[idx]!.toNat!; idx := idx + 1
    let mut adj : Array (Array Nat) := Array.replicate (n + 1) #[]
    for _ in [0:n-1] do
      let u := tokens[idx]!.toNat!
      let v := tokens[idx+1]!.toNat!
      idx := idx + 2
      adj := adj.set! u (adj[u]! |>.push v)
      adj := adj.set! v (adj[v]! |>.push u)
    let paths := solveCase n adj
    IO.println paths.length
    for p in paths do
      IO.println ((p.map (fun x => toString x)).intersperse " ").foldl (· ++ ·) ""
