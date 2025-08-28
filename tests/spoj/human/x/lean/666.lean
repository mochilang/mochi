/- Solution for SPOJ VOCV - Con-Junctions
https://www.spoj.com/problems/VOCV/
-/

import Std
open Std

def modVal : Nat := 10007

private def solveCase (n : Nat) (adj : Array (Array Nat)) : Nat × Nat := Id.run do
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
  let mut dp0 := Array.replicate (n + 1) 0
  let mut dp1 := Array.replicate (n + 1) 0
  let mut dp0c := Array.replicate (n + 1) 0
  let mut dp1c := Array.replicate (n + 1) 0
  let mut i := order.size
  while i > 0 do
    i := i - 1
    let v := order[i]!
    let mut s0 := 0
    let mut c0 := 1
    let mut s1 := 1
    let mut c1 := 1
    for nb in adj[v]! do
      if nb ≠ parent[v]! then
        let s0nb := dp0[nb]!
        let c0nb := dp0c[nb]!
        let s1nb := dp1[nb]!
        let c1nb := dp1c[nb]!
        if s0nb < s1nb then
          s1 := s1 + s0nb
          c1 := (c1 * c0nb) % modVal
        else if s1nb < s0nb then
          s1 := s1 + s1nb
          c1 := (c1 * c1nb) % modVal
        else
          s1 := s1 + s0nb
          c1 := (c1 * ((c0nb + c1nb) % modVal)) % modVal
        s0 := s0 + s1nb
        c0 := (c0 * c1nb) % modVal
    dp0 := dp0.set! v s0
    dp1 := dp1.set! v s1
    dp0c := dp0c.set! v (c0 % modVal)
    dp1c := dp1c.set! v (c1 % modVal)
  let s0r := dp0[1]!
  let c0r := dp0c[1]!
  let s1r := dp1[1]!
  let c1r := dp1c[1]!
  if s0r < s1r then
    (s0r, c0r % modVal)
  else if s1r < s0r then
    (s1r, c1r % modVal)
  else
    (s0r, (c0r + c1r) % modVal)

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
    let (ans, cnt) := solveCase n adj
    IO.println s!"{ans} {cnt}"
