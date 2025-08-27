/- Solution for SPOJ POLY1 - Polygon
https://www.spoj.com/problems/POLY1/
-/

import Std
open Std

structure Tri where
  a : Nat
  b : Nat
  c : Nat

-- build adjacency tree including boundary edge nodes
partial def buildAdj (n : Nat) (tris : Array Tri) : Array (Array Nat) := Id.run do
  let m := tris.size
  let total := m + n
  let mut adj : Array (Array Nat) := Array.mkArray total #[]
  let mut mp : Std.HashMap (Nat × Nat) (Nat × Nat) := {}
  for idx in [0:m] do
    let t := tris.get! idx
    let edges := #[(t.a,t.b),(t.b,t.c),(t.c,t.a)]
    for (u0,v0) in edges do
      let key := if u0 < v0 then (u0,v0) else (v0,u0)
      match mp.find? key with
      | some (j,cnt) =>
          adj := adj.modify idx (fun arr => arr.push j)
          adj := adj.modify j (fun arr => arr.push idx)
          mp := mp.insert key (j, cnt+1)
      | none =>
          mp := mp.insert key (idx,1)
  let mut b := 0
  for (key, val) in mp.toList do
    let tri := val.fst
    let cnt := val.snd
    if cnt = 1 then
      let node := m + b
      b := b + 1
      adj := adj.modify tri (fun arr => arr.push node)
      adj := adj.modify node (fun arr => arr.push tri)
  adj

-- compute candidate from center s
partial def candidate (adj : Array (Array Nat)) (m : Nat) (s : Nat) : Nat := Id.run do
  let total := adj.size
  let mut dist : Array Int := Array.mkArray total (-1)
  let mut first : Array Nat := Array.mkArray total 0
  let mut q : Array Nat := #[]
  dist := dist.set! s 0
  first := first.set! s s
  q := q.push s
  let mut front := 0
  while front < q.size do
    let u := q.get! front
    front := front + 1
    for v in adj.get! u do
      if dist.get! v == -1 then
        dist := dist.set! v (dist.get! u + 1)
        first := first.set! v (if u = s then v else first.get! u)
        q := q.push v
  let mut best : Array Nat := Array.mkArray total 0
  for leaf in [m:total] do
    let d := dist.get! leaf
    if d > 0 then
      let f := first.get! leaf
      if d.toNat > best.get! f then
        best := best.set! f d.toNat
  let mut ds : Array Nat := #[]
  for nb in adj.get! s do
    let d := best.get! nb
    if d > 0 then
      ds := ds.push d
  let ds := ds.qsort (fun a b => a > b)
  if h : 3 ≤ ds.size then
    let a := ds.get! 0
    let b := ds.get! 1
    let c := ds.get! 2
    a + b + c - 2
  else
    0

partial def solveCase (n : Nat) (tris : Array Tri) : Nat :=
  let adj := buildAdj n tris
  let m := tris.size
  let mut ans := 0
  for s in [0:m] do
    let cand := candidate adj m s
    if cand > ans then ans := cand
  ans

partial def parse (toks : Array String) (idx t : Nat) (acc : Array String) : Array String :=
  if t = 0 then acc else
    let n := toks.get! idx |>.toNat!
    let m := n - 2
    let mut tris : Array Tri := #[]
    let mut i := idx + 1
    for _ in [0:m] do
      let a := toks.get! i |>.toNat!
      let b := toks.get! (i+1) |>.toNat!
      let c := toks.get! (i+2) |>.toNat!
      tris := tris.push {a:=a,b:=b,c:=c}
      i := i + 3
    let ans := solveCase n tris
    parse toks i (t-1) (acc.push (toString ans))

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := parse toks 1 t #[]
  for line in outs do
    IO.println line
