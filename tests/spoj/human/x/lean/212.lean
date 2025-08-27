import Std
open Std

/- Solution for SPOJ WATER - Water among Cubes
https://www.spoj.com/problems/WATER/
-/

structure Node where
  h : Nat
  i : Nat
  j : Nat
  deriving Inhabited, DecidableEq

partial def popMin (xs : List Node) : Node × List Node :=
  match xs with
  | [] => (default, [])
  | x :: xs =>
    let rec go (best : Node) (acc : List Node) : List Node -> Node × List Node
      | [] => (best, acc.reverse)
      | y :: ys =>
        if y.h < best.h then
          go y (best :: acc) ys
        else
          go best (y :: acc) ys
    go x [] xs

partial def trap (grid : Array (Array Nat)) (n m : Nat) : Nat :=
  Id.run do
    let mut vis := Array.replicate n (Array.replicate m false)
    let mut pq : List Node := []
    -- initialize borders
    for i in [0:n] do
      let row := grid[i]!
      vis := vis.set! i ((vis[i]!).set! 0 true)
      pq := {h := row[0]!, i := i, j := 0} :: pq
      if m > 1 then
        vis := vis.set! i ((vis[i]!).set! (m-1) true)
        pq := {h := row[m-1]!, i := i, j := m-1} :: pq
    for j in [1:m-1] do
      vis := vis.set! 0 ((vis[0]!).set! j true)
      pq := {h := grid[0]![j]!, i := 0, j := j} :: pq
      vis := vis.set! (n-1) ((vis[n-1]!).set! j true)
      pq := {h := grid[n-1]![j]!, i := n-1, j := j} :: pq
    let mut res := 0
    while pq ≠ [] do
      let (node, pq2) := popMin pq
      pq := pq2
      let h := node.h
      let i := node.i
      let j := node.j
      -- up
      if i > 0 then
        if vis[i-1]![j]! = false then
          vis := vis.set! (i-1) ((vis[i-1]!).set! j true)
          let h2 := grid[i-1]![j]!
          if h > h2 then
            res := res + (h - h2)
            pq := {h := h, i := i-1, j := j} :: pq
          else
            pq := {h := h2, i := i-1, j := j} :: pq
      -- down
      if i + 1 < n then
        if vis[i+1]![j]! = false then
          vis := vis.set! (i+1) ((vis[i+1]!).set! j true)
          let h2 := grid[i+1]![j]!
          if h > h2 then
            res := res + (h - h2)
            pq := {h := h, i := i+1, j := j} :: pq
          else
            pq := {h := h2, i := i+1, j := j} :: pq
      -- left
      if j > 0 then
        if vis[i]![j-1]! = false then
          vis := vis.set! i ((vis[i]!).set! (j-1) true)
          let h2 := grid[i]![j-1]!
          if h > h2 then
            res := res + (h - h2)
            pq := {h := h, i := i, j := j-1} :: pq
          else
            pq := {h := h2, i := i, j := j-1} :: pq
      -- right
      if j + 1 < m then
        if vis[i]![j+1]! = false then
          vis := vis.set! i ((vis[i]!).set! (j+1) true)
          let h2 := grid[i]![j+1]!
          if h > h2 then
            res := res + (h - h2)
            pq := {h := h, i := i, j := j+1} :: pq
          else
            pq := {h := h2, i := i, j := j+1} :: pq
    return res

partial def readMatrix (nums : Array Nat) (idx n m i j : Nat)
    (grid : Array (Array Nat)) : (Array (Array Nat) × Nat) :=
  if i < n then
    if j < m then
      let val := nums[idx]!
      let row := (grid[i]!).set! j val
      let grid := grid.set! i row
      readMatrix nums (idx+1) n m i (j+1) grid
    else
      readMatrix nums idx n m (i+1) 0 grid
  else
    (grid, idx)

partial def parseCases (nums : Array Nat) (idx t : Nat) (acc : Array String)
    : Array String :=
  if t = 0 then acc
  else
    let n := nums[idx]!
    let m := nums[idx+1]!
    let (grid, nextIdx) :=
      readMatrix nums (idx+2) n m 0 0 (Array.replicate n (Array.replicate m 0))
    let ans := trap grid n m
    parseCases nums nextIdx (t-1) (acc.push (toString ans))

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
  let toksArr : Array String := List.toArray toks
  let nums := toksArr.map (fun s => s.toNat!)
  let t := nums[0]!
  let res := parseCases nums 1 t #[]
  for line in res do
    IO.println line
