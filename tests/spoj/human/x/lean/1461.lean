/- Solution for SPOJ DRAGON - Greedy Hydra
https://www.spoj.com/problems/DRAGON/
-/

import Std
open Std

def INF : Int := 1000000000000000

def mkMatrix (r c : Nat) (val : Int) : Array (Array Int) :=
  Array.replicate r (Array.replicate c val)

structure DPState where
  dp   : Array (Array Int)
  best : Array Int
  size : Nat

def baseState (K M : Nat) : DPState :=
  let mut dp := mkMatrix (K+1) (M+1) INF
  let row := (dp.get! 1).set! 1 0
  dp := dp.set! 1 row
  let mut best := Array.mkArray (M+1) INF
  best := best.set! 1 0
  { dp := dp, best := best, size := 1 }

def combine (a b : DPState) (w : Int) (K M : Nat) : DPState := Id.run do
  let sizeU := a.size
  let sizeV := b.size
  let maxSU := min K sizeU
  let maxSV := min K sizeV
  let mut dp := mkMatrix (K+1) (M+1) INF
  for su in [1:maxSU+1] do
    for gu in [1:M+1] do
      let valU := (a.dp.get! su).get! gu
      if valU < INF then
        let maxSv := min maxSV (K - su)
        for sv in [1:maxSv+1] do
          for gv in [1:M+1] do
            let valV := (b.dp.get! sv).get! gv
            if valV < INF then
              let sNew := su + sv
              let gNew := gu + gv - 1
              if gNew ≤ M then
                let row := dp.get! sNew
                let old := row.get! gNew
                let row := row.set! gNew (min old (valU + valV))
                dp := dp.set! sNew row
        for gv in [1:M+1] do
          let gNew := gu + gv
          if gNew ≤ M then
            let valV := b.best.get! gv
            if valV < INF then
              let row := dp.get! su
              let old := row.get! gNew
              let row := row.set! gNew (min old (valU + valV + w))
              dp := dp.set! su row
  let mut best := Array.mkArray (M+1) INF
  let maxSNew := min K (sizeU + sizeV)
  for g in [1:M+1] do
    for s in [1:maxSNew+1] do
      let v := (dp.get! s).get! g
      if v < best.get! g then
        best := best.set! g v
  return { dp := dp, best := best, size := sizeU + sizeV }

partial def dfs (u parent : Nat) (adj : Array (List (Nat × Int))) (K M : Nat) : DPState :=
  Id.run do
    let mut st := baseState K M
    for (v,w) in adj.get! u do
      if v ≠ parent then
        let child := dfs v u adj K M
        st := combine st child w K M
    return st

def buildAdj (N : Nat) (edges : List (Nat × Nat × Int)) :
    Array (List (Nat × Int)) := Id.run do
  let mut adj : Array (List (Nat × Int)) := Array.replicate (N+1) []
  for (u,v,w) in edges do
    adj := adj.set! u ((v,w) :: adj.get! u)
    adj := adj.set! v ((u,w) :: adj.get! v)
  return adj

def solveOne (N M K : Nat) (edges : List (Nat × Nat × Int)) : Int :=
  if M > N || K > N then
    -1
  else
    let adj := buildAdj N edges
    let st := dfs 1 0 adj K M
    let ans := (st.dp.get! K).get! M
    if ans ≥ INF then -1 else ans

def readInts : IO (Array Int) := do
  let s ← IO.FS.Stream.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

partial def process (data : Array Int) (idx : Nat) : IO Unit := do
  if h : idx < data.size then
    let N := Nat.ofInt (data.get! idx)
    let M := Nat.ofInt (data.get! (idx+1))
    let K := Nat.ofInt (data.get! (idx+2))
    let mut i := idx + 3
    let mut edges : List (Nat × Nat × Int) := []
    for _ in [0:N-1] do
      let u := Nat.ofInt (data.get! i)
      let v := Nat.ofInt (data.get! (i+1))
      let w := data.get! (i+2)
      edges := (u,v,w) :: edges
      i := i + 3
    IO.println (solveOne N M K edges.reverse)
    process data i
  else
    pure ()

def main : IO Unit := do
  let arr ← readInts
  process arr 0
