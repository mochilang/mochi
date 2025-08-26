/- Solution for SPOJ INCARDS - Invitation Cards
https://www.spoj.com/problems/INCARDS/
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

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let parts := line.splitOn " "
      let p := parts.get! 0 |>.toNat!
      let q := parts.get! 1 |>.toNat!
      let mut g : Array (Array Edge) := Array.mkArray (p + 1) #[]
      let mut rg : Array (Array Edge) := Array.mkArray (p + 1) #[]
      for _ in [0:q] do
        let ln := (← h.getLine).trim
        let ps := ln.splitOn " "
        let a := ps.get! 0 |>.toNat!
        let b := ps.get! 1 |>.toNat!
        let w := ps.get! 2 |>.toNat!
        g := g.set! a (g.get! a |>.push {to := b, cost := w})
        rg := rg.set! b (rg.get! b |>.push {to := a, cost := w})
      let d1 := dijkstra g p 1
      let d2 := dijkstra rg p 1
      let mut total := 0
      for i in [1:p+1] do
        total := total + d1.get! i + d2.get! i
      IO.println total
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
