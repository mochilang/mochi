/- Solution for SPOJ SERVERS - Interesting server data
https://www.spoj.com/problems/SERVERS/
-/

import Std
open Std

structure Edge where
  to : Nat
  cost : Nat

def dijkstra (adj : Array (Array Edge)) (n src : Nat) : Array Nat :=
  let inf := (1 <<< 60)
  let mut dist := Array.mkArray (n + 1) inf
  let mut used := Array.mkArray (n + 1) false
  dist := dist.set! src 0
  for _ in [0:n] do
    let mut u := 0
    let mut best := inf
    for i in [1:n+1] do
      if !used.get! i && dist.get! i < best then
        best := dist.get! i
        u := i
    if u == 0 then
      break
    used := used.set! u true
    for e in adj.get! u do
      let v := e.to
      let w := e.cost
      let alt := best + w
      if alt < dist.get! v then
        dist := dist.set! v alt
  dist

def countInteresting (dist ranks : Array Nat) : Nat :=
  let n := dist.size
  let mut pairs : List (Nat × Nat) := []
  for i in [0:n] do
    pairs := (dist.get! i, ranks.get! i) :: pairs
  let sorted := pairs.qsort (fun a b => a.1 < b.1)
  let rec loop (lst : List (Nat × Nat)) (cur : Nat) (acc : Nat) : Nat :=
    match lst with
    | [] => acc
    | (d,r)::rest =>
      let (same, rest2) := rest.span (fun p => p.1 = d)
      let group := (d,r)::same
      let (gm, cnt) := group.foldl
        (fun (s : Nat × Nat) p =>
          let m := s.1
          let c := s.2
          let rk := p.2
          if rk > m then (rk, 1)
          else if rk = m then (m, c + 1)
          else (m, c))
        (0,0)
      if gm < cur then loop rest2 cur acc
      else loop rest2 gm (acc + cnt)
  loop sorted 0 0

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let parts := line.splitOn " "
      let n := parts.get! 0 |>.toNat!
      let m := parts.get! 1 |>.toNat!
      let mut ranks : Array Nat := Array.mkArray (n + 1) 0
      for i in [1:n+1] do
        let r := (← h.getLine).trim.toNat!
        ranks := ranks.set! i r
      let mut adj : Array (Array Edge) := Array.mkArray (n + 1) #[]
      for _ in [0:m] do
        let ln := (← h.getLine).trim
        let ps := ln.splitOn " "
        let a := ps.get! 0 |>.toNat!
        let b := ps.get! 1 |>.toNat!
        let w := ps.get! 2 |>.toNat!
        adj := adj.set! a (adj.get! a |>.push {to := b, cost := w})
        adj := adj.set! b (adj.get! b |>.push {to := a, cost := w})
      let mut total := 0
      for v in [1:n+1] do
        let dist := dijkstra adj n v
        total := total + countInteresting dist ranks
      IO.println total
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
