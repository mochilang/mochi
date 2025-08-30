/- Solution for SPOJ RELINETS - Reliable Nets
https://www.spoj.com/problems/RELINETS/
-/

import Std
open Std

structure Edge where
  u : Nat
  v : Nat
  w : Nat

def buildAdj (n : Nat) (edges : Array Edge) (mask : Nat) (skip : Option Nat := none) : Array (List Nat) :=
  Id.run do
    let mut adj : Array (List Nat) := Array.mkArray (n+1) []
    for i in [0:edges.size] do
      if Nat.testBit mask i && skip ≠ some i then
        let e := edges[i]!
        adj := adj.set! e.u (e.v :: adj[e.u]!)
        adj := adj.set! e.v (e.u :: adj[e.v]!)
    return adj

partial def dfsTraverse (adj : Array (List Nat)) : List Nat → Array Bool → Array Bool
| [], vis => vis
| v :: rest, vis =>
    let (vis', st') := adj[v]!.foldl (fun (p : Array Bool × List Nat) w =>
        let (vis, st) := p
        if vis[w]! then (vis, st) else (vis.set! w true, w :: st)) (vis, rest)
    dfsTraverse adj st' vis'

def connected? (n : Nat) (adj : Array (List Nat)) : Bool :=
  let start? := (List.range (n+1)).find? (fun i => i > 0 && adj[i]! ≠ [])
  match start? with
  | none => false
  | some s =>
      let vis := dfsTraverse adj [s] ((Array.mkArray (n+1) false).set! s true)
      let rec check (i : Nat) : Bool :=
        if h : i ≤ n then
          if vis[i]! then check (i+1) else false
        else
          true
      check 1

def isReliable (n : Nat) (edges : Array Edge) (mask : Nat) : Bool :=
  let adj := buildAdj n edges mask none
  if !connected? n adj then false else
    Id.run do
      let mut ok := true
      for i in [0:edges.size] do
        if ok && Nat.testBit mask i then
          let adj2 := buildAdj n edges mask (some i)
          if !connected? n adj2 then
            ok := false
      return ok

def minimalCost (n m : Nat) (edges : Array Edge) : Option Nat :=
  let limit := Nat.shiftLeft 1 m
  Id.run do
    let mut best : Option Nat := none
    for mask in [0:limit] do
      let mut cnt := 0
      let mut cost := 0
      for i in [0:m] do
        if Nat.testBit mask i then
          cnt := cnt + 1
          cost := cost + (edges[i]!.w)
      if cnt ≥ n then
        match best with
        | some b =>
            if cost < b && isReliable n edges mask then
              best := some cost
        | none =>
            if isReliable n edges mask then
              best := some cost
    return best

partial def solveAll (toks : Array String) (idx case : Nat) (acc : List String) : List String :=
  let n := toks[idx]!.toNat!
  let m := toks[idx+1]!.toNat!
  if n = 0 ∧ m = 0 then acc.reverse else
    let (edges, j) := Id.run do
      let mut arr : Array Edge := #[]
      let mut j := idx + 2
      for _ in [0:m] do
        let u := toks[j]!.toNat!
        let v := toks[j+1]!.toNat!
        let w := toks[j+2]!.toNat!
        arr := arr.push {u := u, v := v, w := w}
        j := j + 3
      return (arr, j)
    match minimalCost n m edges with
    | some c =>
        solveAll toks j (case+1) (s!"The minimal cost for test case {case} is {c}." :: acc)
    | none =>
        solveAll toks j (case+1) (s!"There is no reliable net possible for test case {case}." :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
               |>.filter (· ≠ "")
               |>.toArray
  let outs := solveAll toks 0 1 []
  IO.println (String.intercalate "\n" outs)
