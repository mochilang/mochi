/- Solution for SPOJ CHRIS - On the Way to Find Chris
https://www.spoj.com/problems/CHRIS/
-/
import Std
open Std

-- read all tokens as natural numbers
private def readTokens : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let parts := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

-- first DFS: compute down distances and parent
private partial def dfsDown (adj : Array (Array (Nat × Nat)))
    (v parent : Nat) (downRef parRef : IO.Ref (Array Nat)) : IO Nat := do
  let mut best : Nat := 0
  for (u,w) in adj[v]! do
    if u ≠ parent then
      let d ← dfsDown adj u v downRef parRef
      let total := d + w
      if total > best then
        best := total
  let dArr ← downRef.get
  let pArr ← parRef.get
  downRef.set (dArr.set! v best)
  parRef.set (pArr.set! v parent)
  return best

-- second DFS: compute up distances
private partial def dfsUp (adj : Array (Array (Nat × Nat)))
    (v parent : Nat) (down : Array Nat) (upRef : IO.Ref (Array Nat)) : IO Unit := do
  let edges := adj[v]!
  -- compute top two child lengths
  let mut first : Nat := 0
  let mut second : Nat := 0
  for (u,w) in edges do
    if u ≠ parent then
      let len := down[u]! + w
      if len ≥ first then
        second := first
        first := len
      else if len > second then
        second := len
  let upv := (← upRef.get)[v]!
  for (u,w) in edges do
    if u ≠ parent then
      let use := if down[u]! + w = first then second else first
      let candidate := Nat.max upv use
      upRef.modify fun arr => arr.set! u (candidate + w)
      dfsUp adj u v down upRef
  pure ()

-- compute answer from down and up arrays
private def computeAnswer (adj : Array (Array (Nat × Nat)))
    (down up par : Array Nat) (n : Nat) : Nat := Id.run do
  let mut ans : Nat := 0
  for v in [1:n+1] do
    let mut a : Nat := 0
    let mut b : Nat := 0
    let mut c : Nat := 0
    -- include path via parent
    let lensViaParent := up[v]!
    let mut update := fun (len : Nat) =>
      if len ≥ a then
        c := b; b := a; a := len
      else if len ≥ b then
        c := b; b := len
      else if len > c then
        c := len
    update lensViaParent
    for (u,w) in adj[v]! do
      if u ≠ par[v]! then
        update (down[u]! + w)
    let cand := a + 2*b + c
    if cand > ans then
      ans := cand
  ans

-- solve single test case
private def solveCase (n m : Nat) (edges : Array (Nat × Nat × Nat)) : IO Nat := do
  let mut adj : Array (Array (Nat × Nat)) := Array.replicate (n+1) #[]
  for (x,y,z) in edges do
    adj := adj.set! x ((adj[x]!).push (y,z))
    adj := adj.set! y ((adj[y]!).push (x,z))
  let downRef ← IO.mkRef (Array.replicate (n+1) 0)
  let parRef ← IO.mkRef (Array.replicate (n+1) 0)
  let _ ← dfsDown adj 1 0 downRef parRef
  let down ← downRef.get
  let par ← parRef.get
  let upRef ← IO.mkRef (Array.replicate (n+1) 0)
  dfsUp adj 1 0 down upRef
  let up ← upRef.get
  return computeAnswer adj down up par n

-- main logic
def main : IO Unit := do
  let tokens ← readTokens
  let mut idx : Nat := 0
  let t := tokens[idx]!; idx := idx + 1
  for _ in [0:t] do
    let n := tokens[idx]!; idx := idx + 1
    let m := tokens[idx]!; idx := idx + 1
    let mut edges : Array (Nat × Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let x := tokens[idx]!; let y := tokens[idx+1]!; let z := tokens[idx+2]!; idx := idx + 3
      edges := edges.push (x,y,z)
    let ans ← solveCase n m edges
    IO.println ans
  pure ()
