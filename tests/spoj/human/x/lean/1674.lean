/- Solution for SPOJ EXPLOSN - The Explosion
https://www.spoj.com/problems/EXPLOSN/
-/
import Std
open Std

/-- Read all natural numbers from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.FS.readFile "/dev/stdin"
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  pure <| toks.filterMap String.toNat?

/-- Solve one test case with `n` people and testimonies `p` (1-based). -/
partial def solveCase (n : Nat) (p1 : Array Nat) : Nat :=
  let p := p1.map (· - 1)
  -- build reverse adjacency and indegrees
  let mut rev : Array (List Nat) := Array.mkArray n []
  let mut indeg : Array Nat := Array.mkArray n 0
  for i in [0:n] do
    let v := p.get! i
    indeg := indeg.modify v (· + 1)
    rev := rev.modify v (fun l => i :: l)
  -- process tree nodes
  let mut dp0 : Array Nat := Array.mkArray n 0
  let mut dp1 : Array Nat := Array.mkArray n 0
  let mut removed : Array Bool := Array.mkArray n false
  let mut q : List Nat := []
  for i in [0:n] do
    if indeg.get! i == 0 then q := i :: q
  let rec proc (q : List Nat) (indeg dp0 dp1 : Array Nat) (removed : Array Bool)
      : (Array Nat × Array Nat × Array Nat × Array Bool) :=
    match q with
    | [] => (indeg, dp0, dp1, removed)
    | u :: qs =>
        let (d0, d1) := (rev.get! u).foldl
          (fun (a,b) v => (a + dp1.get! v, b + Nat.min (dp0.get! v) (dp1.get! v)))
          (0,1)
        let dp0 := dp0.set! u d0
        let dp1 := dp1.set! u d1
        let removed := removed.set! u true
        let parent := p.get! u
        let indeg := indeg.modify parent (· - 1)
        let qs := if indeg.get! parent == 0 then parent :: qs else qs
        proc qs indeg dp0 dp1 removed
  let (indeg, dp0, dp1, removed) := proc q indeg dp0 dp1 removed
  -- mark cycle nodes
  let mut inCycle : Array Bool := Array.mkArray n false
  for i in [0:n] do
    if !(removed.get! i) then inCycle := inCycle.set! i true
  -- compute dp for cycle nodes using only tree children
  for i in [0:n] do
    if inCycle.get! i then
      let (d0, d1) := (rev.get! i).foldl
        (fun (a,b) v =>
          if inCycle.get! v then (a,b)
          else (a + dp1.get! v, b + Nat.min (dp0.get! v) (dp1.get! v)))
        (0,1)
      dp0 := dp0.set! i d0
      dp1 := dp1.set! i d1
  -- solve cycles
  let mut visited : Array Bool := Array.mkArray n false
  let inf : Nat := 1000000000
  let solveCycle (cycle : Array Nat) : Nat :=
    let k := cycle.size
    if k == 1 then
      dp1.get! (cycle.get! 0)
    else
      -- case 1: first selected
      let mut sel := dp1.get! (cycle.get! 0)
      let mut not := inf
      for i in [1:k] do
        let idx := cycle.get! i
        let a := dp0.get! idx
        let b := dp1.get! idx
        let newSel := Nat.min sel not + b
        let newNot := sel + a
        sel := newSel
        not := newNot
      let case1 := Nat.min sel not
      -- case 2: first not selected
      let mut sel2 := dp0.get! (cycle.get! 0) + dp1.get! (cycle.get! 1)
      let mut not2 := inf
      for i in [2:k] do
        let idx := cycle.get! i
        let a := dp0.get! idx
        let b := dp1.get! idx
        let newSel := Nat.min sel2 not2 + b
        let newNot := sel2 + a
        sel2 := newSel
        not2 := newNot
      let case2 := sel2
      Nat.min case1 case2
  let mut ans := 0
  for i in [0:n] do
    if inCycle.get! i && !(visited.get! i) then
      let rec build (cur : Nat) (cycle : Array Nat) (visited : Array Bool)
          : (Array Nat × Array Bool) :=
        if visited.get! cur then (cycle, visited)
        else
          let cycle := cycle.push cur
          let visited := visited.set! cur true
          build (p.get! cur) cycle visited
      let (cycle, visited') := build i #[] visited
      visited := visited'
      ans := ans + solveCycle cycle
  ans

/-- Main entry point. -/
def main : IO Unit := do
  let nums ← readInts
  let t := nums.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let n := nums.get! idx
    idx := idx + 1
    let mut p : Array Nat := Array.mkArray n 0
    for i in [0:n] do
      p := p.set! i (nums.get! idx)
      idx := idx + 1
    let ans := solveCase n p
    IO.println ans
