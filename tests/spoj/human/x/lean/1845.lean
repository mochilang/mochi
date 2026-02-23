/- Solution for SPOJ MICEMAZE - Mice and Maze
https://www.spoj.com/problems/MICEMAZE/
-/
import Std
open Std

structure Edge where
  to : Nat
  cost : Nat

def dijkstra (adj : Array (Array Edge)) (n start : Nat) : Array Nat :=
  Id.run do
    let inf := 1000000000
    let mut dist := Array.mkArray (n + 1) inf
    let mut used := Array.mkArray (n + 1) false
    dist := dist.set! start 0
    for _ in [0:n] do
      let mut u := 0
      let mut best := inf
      for i in [1:n+1] do
        if !used.get! i && dist.get! i < best then
          best := dist.get! i
          u := i
      if u = 0 then
        break
      used := used.set! u true
      for e in adj.get! u do
        let v := e.to
        let w := e.cost
        let alt := best + w
        if alt < dist.get! v then
          dist := dist.set! v alt
    return dist

def main : IO Unit := do
  let h ← IO.getStdin
  let input ← h.readToEnd
  let toks := input.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let nums := toks.map String.toNat!
  let n := nums.get! 0
  let e := nums.get! 1
  let t := nums.get! 2
  let m := nums.get! 3
  let mut idx := 4
  let mut adj : Array (Array Edge) := Array.mkArray (n + 1) #[]
  for _ in [0:m] do
    let a := nums.get! idx
    let b := nums.get! (idx+1)
    let w := nums.get! (idx+2)
    idx := idx + 3
    adj := adj.set! b (adj.get! b |>.push {to := a, cost := w})
  let dist := dijkstra adj n e
  let mut cnt := 0
  for i in [1:n+1] do
    if dist.get! i ≤ t then
      cnt := cnt + 1
  IO.println cnt
