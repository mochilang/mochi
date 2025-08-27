/- Solution for SPOJ EOPERA - Exchange Operations
https://www.spoj.com/problems/EOPERA/
-/

import Std
open Std

/-- Encode permutation of 12 numbers into a single natural number using base 12. --/
private def encode (a : Array Nat) : Nat :=
  a.foldl (fun acc v => acc * 12 + v) 0

/-- Swap elements at positions `i` and `j` in array `a`. --/
private def swap (a : Array Nat) (i j : Nat) : Array Nat :=
  let vi := a[i]!
  let vj := a[j]!
  (a.set! i vj).set! j vi

/-- Precomputed adjacency for the position of zero. --/
private def adj : Array (List Nat) := #[
  [1,3,6],          -- 0
  [0,2,4,7],        -- 1
  [1,5,8],          -- 2
  [4,0,9],          -- 3
  [3,5,1,10],       -- 4
  [4,2,11],         -- 5
  [7,9,0],          -- 6
  [6,8,10,1],       -- 7
  [7,11,2],         -- 8
  [10,6,3],         -- 9
  [9,11,7,4],       -- 10
  [10,8,5]          -- 11
]

/-- Breadth-first search to compute minimal swaps to reach the sorted sequence. --/
private def bfs (start : Array Nat) : Nat := Id.run do
  let goalArr : Array Nat := Array.range 12
  let goalCode := encode goalArr
  let startCode := encode start
  if startCode = goalCode then
    return 0
  let mut visited : Std.HashSet Nat := {}
  visited := visited.insert startCode
  let mut q : Array (Array Nat × Nat) := #[(start, 0)]
  let mut idx : Nat := 0
  while idx < q.size do
    let (state, dist) := q[idx]!
    idx := idx + 1
    -- find position of zero
    let mut z : Nat := 0
    for i in [0:state.size] do
      if state[i]! = 0 then
        z := i
    -- explore neighbors
    for j in adj[z]! do
      let st2 := swap state z j
      let code := encode st2
      if code = goalCode then
        return dist + 1
      if !visited.contains code then
        visited := visited.insert code
        q := q.push (st2, dist + 1)
  return 0

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

/-- Solve from given token array starting at index `i`. Returns output and next index. --/
private def solveCase (data : Array Nat) (i : Nat) : (String × Nat) :=
  let arr := data.extract i (i + 12)
  let ans := bfs arr
  (toString ans, i + 12)

/-- Main program: read input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  let t := data[0]!
  let mut idx := 1
  for _ in [0:t] do
    let (out, idx') := solveCase data idx
    IO.println out
    idx := idx'
