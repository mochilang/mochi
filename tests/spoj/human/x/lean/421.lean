/- Solution for SPOJ AROAD - Another Road Problem
https://www.spoj.com/problems/AROAD/
-/

import Std
open Std

structure Edge where
  u : Nat
  v : Nat
  w : Int
  deriving Inhabited

/- Parse all natural numbers from stdin. -/
def readNats : IO (Array Nat) := do
  let data ← (← IO.getStdin).readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c >= '0' && c <= '9' then
      num := num * 10 + (c.toNat - '0'.toNat)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then
    arr := arr.push num
  return arr

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, p2) := find parent px
    (r, p2.set! x r)

def union (parent : Array Nat) (a b : Nat) : (Bool × Array Nat) :=
  let (ra, p1) := find parent a
  let (rb, p2) := find p1 b
  if ra = rb then
    (false, p2)
  else
    (true, p2.set! ra rb)

def mst (n : Nat) (edges : Array Edge) (lam : Int) : Option (Nat × Int) :=
  Id.run do
    let sorted := edges.qsort (fun e1 e2 =>
      (e1.w + (if e1.u == 1 || e1.v == 1 then lam else 0)) <
      (e2.w + (if e2.u == 1 || e2.v == 1 then lam else 0)))
    let mut parent := Array.replicate (n + 1) 0
    for i in [0:n+1] do
      parent := parent.set! i i
    let mut deg : Nat := 0
    let mut cost : Int := 0
    let mut cnt : Nat := 0
    for e in sorted do
      let (added, p') := union parent e.u e.v
      parent := p'
      if added then
        cnt := cnt + 1
        cost := cost + e.w
        if e.u == 1 || e.v == 1 then
          deg := deg + 1
    if cnt = n - 1 then
      return some (deg, cost)
    else
      return none

partial def search (n d : Nat) (edges : Array Edge) (lo hi : Int) (best : Option Int) : Option Int :=
  if lo > hi then
    best
  else
    let mid := (lo + hi) / 2
    match mst n edges mid with
    | none => search n d edges lo (mid - 1) best
    | some (deg, cost) =>
        if deg <= d then
          let best' := match best with
            | some b => some (if cost < b then cost else b)
            | none => some cost
          search n d edges lo (mid - 1) best'
        else
          search n d edges (mid + 1) hi best

partial def solveCase (n : Nat) (d : Nat) (edges : Array Edge) : Option Int :=
  search n d edges 0 20000 none

/-- Process all test cases. --/
def process (data : Array Nat) : String :=
  Id.run do
    let t := data[0]!
    let mut idx := 1
    let mut out := ""
    for case in [0:t] do
      let n := data[idx]!; idx := idx + 1
      let m := data[idx]!; idx := idx + 1
      let d := data[idx]!; idx := idx + 1
      let mut edges : Array Edge := Array.replicate m default
      for i in [0:m] do
        let u := data[idx]!; idx := idx + 1
        let v := data[idx]!; idx := idx + 1
        let w := Int.ofNat data[idx]!; idx := idx + 1
        edges := edges.set! i {u := u, v := v, w := w}
      match solveCase n d edges with
      | some cost => out := out ++ toString cost
      | none => out := out ++ "NONE"
      if case + 1 < t then
        out := out ++ "\n"
    return out

def main : IO Unit := do
  let data ← readNats
  IO.println (process data)
