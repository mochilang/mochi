/- Solution for SPOJ NETADMIN - Network Administrator
https://www.spoj.com/problems/NETADMIN/
-/

import Std
open Std

/-- Build capacity and adjacency structures for given capacity value. --/
private def build (n : Nat) (edges : Array (Nat × Nat)) (targets : Array Nat) (capVal : Nat) :
    (Array (Array Int) × Array (List Nat)) := Id.run do
  let size := n + 2
  let mut cap : Array (Array Int) := Array.replicate size (Array.replicate size 0)
  let mut adj : Array (List Nat) := Array.replicate size []
  for e in edges do
    let u := e.fst; let v := e.snd
    let rowU := cap.get! u
    cap := cap.set! u (rowU.set! v ((rowU.get! v) + Int.ofNat capVal))
    let rowV := cap.get! v
    cap := cap.set! v (rowV.set! u ((rowV.get! u) + Int.ofNat capVal))
    adj := adj.set! u (v :: adj.get! u)
    adj := adj.set! v (u :: adj.get! v)
  let sink := n + 1
  for h in targets do
    let rowH := cap.get! h
    cap := cap.set! h (rowH.set! sink ((rowH.get! sink) + 1))
    adj := adj.set! h (sink :: adj.get! h)
    adj := adj.set! sink (h :: adj.get! sink)
  return (cap, adj)

/-- BFS augmenting path sending one unit from s to t. --/
private partial def bfsAug (cap : Array (Array Int)) (adj : Array (List Nat)) (s t size : Nat) :
    Option (Array (Array Int)) :=
  let rec bfs (q : List Nat) (par : Array (Option Nat)) : Option (Array (Option Nat)) :=
    match q with
    | [] => none
    | v :: qs =>
        if v = t then some par else
        let rec scan (nbrs : List Nat) (qs : List Nat) (par : Array (Option Nat)) : Option (Array (Option Nat)) :=
          match nbrs with
          | [] => bfs qs par
          | w :: ws =>
              if par.get! w = none && (cap.get! v).get! w > 0 then
                scan ws (qs ++ [w]) (par.set! w (some v))
              else
                scan ws qs par
        scan (adj.get! v) qs par
  match bfs [s] ((Array.mkArray size none).set! s (some s)) with
  | none => none
  | some par =>
      match par.get! t with
      | none => none
      | some _ =>
          let mut cap2 := cap
          let mut v := t
          while v ≠ s do
            let u := (par.get! v).get!
            let rowU := cap2.get! u
            let rowV := cap2.get! v
            cap2 := cap2.set! u (rowU.set! v ((rowU.get! v) - 1))
            cap2 := cap2.set! v (rowV.set! u ((rowV.get! u) + 1))
            v := u
          some cap2

/-- Check if we can connect all targets with edge capacity `C`. --/
private partial def canRoute (n : Nat) (edges : Array (Nat × Nat)) (targets : Array Nat)
    (k C : Nat) : Bool :=
  let (cap0, adj) := build n edges targets C
  let size := n + 2
  let sink := n + 1
  let rec loop (cap : Array (Array Int)) (sent : Nat) : Bool :=
    if sent = k then true
    else
      match bfsAug cap adj 1 sink size with
      | some cap' => loop cap' (sent + 1)
      | none      => false
  loop cap0 0

/-- Solve a single test case. Returns answer and next index. --/
private partial def solveCase (data : Array Nat) (idx : Nat) : (Nat × Nat) :=
  let n := data[idx]!
  let m := data[idx+1]!
  let k := data[idx+2]!
  let mut i := idx + 3
  let mut targets : Array Nat := #[]
  for _ in [0:k] do
    targets := targets.push (data[i]!); i := i + 1
  let mut edges : Array (Nat × Nat) := #[]
  for _ in [0:m] do
    let a := data[i]!; let b := data[i+1]!; i := i + 2
    edges := edges.push (a, b)
  let rec bin (lo hi : Nat) : Nat :=
    if lo < hi then
      let mid := (lo + hi) / 2
      if canRoute n edges targets k mid then
        bin lo mid
      else
        bin (mid + 1) hi
    else lo
  let ans := if k = 0 then 0 else bin 1 k
  (ans, i)

/-- Read all integers from stdin. --/
private def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Main entry: process test cases. --/
def main : IO Unit := do
  let data ← readInts
  let t := data[0]!
  let mut idx := 1
  for _ in [0:t] do
    let (ans, idx') := solveCase data idx
    IO.println ans
    idx := idx'
