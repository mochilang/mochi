/- Solution for SPOJ PT07X - Vertex Cover
https://www.spoj.com/problems/PT07X/
-/

import Std
open Std

private def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array String := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p
  return arr

def main : IO Unit := do
  let tokens ← readTokens
  let mut idx : Nat := 0
  if idx < tokens.size then
    let n := tokens[idx]!.toNat!
    idx := idx + 1
    let mut adj : Array (Array Nat) := Array.replicate (n + 1) #[]
    for _ in [0:n-1] do
      let u := tokens[idx]!.toNat!
      let v := tokens[idx+1]!.toNat!
      idx := idx + 2
      adj := adj.set! u (adj[u]! |>.push v)
      adj := adj.set! v (adj[v]! |>.push u)
    let mut parent : Array Nat := Array.replicate (n + 1) 0
    let mut order : Array Nat := #[]
    let mut stack : List (Nat × Nat) := [(1, 0)]
    while stack ≠ [] do
      match stack with
      | (v, p) :: rest =>
          stack := rest
          parent := parent.set! v p
          order := order.push v
          for nb in adj[v]! do
            if nb ≠ p then
              stack := (nb, v) :: stack
      | [] => ()
    let mut dp0 : Array Nat := Array.replicate (n + 1) 0
    let mut dp1 : Array Nat := Array.replicate (n + 1) 0
    let mut i := order.size
    while i > 0 do
      i := i - 1
      let v := order[i]!
      let mut include := 1
      let mut exclude := 0
      for nb in adj[v]! do
        if nb ≠ parent[v]! then
          include := include + Nat.min (dp0[nb]!) (dp1[nb]!)
          exclude := exclude + dp1[nb]!
      dp1 := dp1.set! v include
      dp0 := dp0.set! v exclude
    let ans := Nat.min (dp0[1]!) (dp1[1]!)
    IO.println ans
  else
    pure ()
