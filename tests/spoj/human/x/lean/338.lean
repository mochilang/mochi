/- Solution for SPOJ ROADS - Roads
https://www.spoj.com/problems/ROADS/
-/

import Std
open Std

structure Edge where
  to : Nat
  len : Nat
  toll : Nat
deriving Repr

def INF : Nat := 1000000000

/-- solve a single test case -/
def solveCase (K N : Nat) (adj : Array (Array Edge)) : Int :=
  Id.run do
    let mut dist : Array (Array Nat) := Array.replicate (N + 1) (Array.replicate (K + 1) INF)
    let row := dist[1]!
    dist := dist.set! 1 (row.set! 0 0)
    for cost in [0:K+1] do
      for city in [1:N+1] do
        let cur := (dist[city]!)[cost]!
        if cur < INF then
          for e in adj[city]! do
            let nc := cost + e.toll
            if nc ≤ K then
              let nd := cur + e.len
              let old := (dist[e.to]!)[nc]!
              if nd < old then
                dist := dist.set! e.to ((dist[e.to]!).set! nc nd)
    let arr := dist[N]!
    let mut ans := INF
    for i in [0:K+1] do
      let v := arr[i]!
      if v < ans then
        ans := v
    if ans == INF then
      return (-1)
    else
      return Int.ofNat ans

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let K := (← h.getLine).trim.toNat!
    let N := (← h.getLine).trim.toNat!
    let R := (← h.getLine).trim.toNat!
    let mut adj : Array (Array Edge) := Array.replicate (N + 1) #[]
    for _ in [0:R] do
      let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
      let s := parts[0]! |>.toNat!
      let d := parts[1]! |>.toNat!
      let l := parts[2]! |>.toNat!
      let tol := parts[3]! |>.toNat!
      adj := adj.set! s ((adj[s]!).push {to := d, len := l, toll := tol})
    let ans := solveCase K N adj
    IO.println ans
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
