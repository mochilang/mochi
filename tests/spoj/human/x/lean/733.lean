/- Solution for SPOJ MTWALK - Mountain Walking
https://www.spoj.com/problems/MTWALK/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- BFS within altitude range [lo, hi]. --/
partial def bfs (grid : Array (Array Nat)) (n lo hi : Nat) : Bool :=
  let start := grid[0]![0]!
  if start < lo || start > hi then
    false
  else
    let rec loop (q : List (Nat × Nat)) (vis : Std.HashSet Nat) : Bool :=
      match q with
      | [] => false
      | (x,y) :: qs =>
        let idx := x * n + y
        if vis.contains idx then
          loop qs vis
        else if x == n-1 && y == n-1 then
          true
        else
          let vis := vis.insert idx
          let up := if x = 0 then [] else
                       let nx := x - 1
                       let hgt := grid[nx]![y]!
                       if hgt ≥ lo && hgt ≤ hi then [(nx, y)] else []
          let down := if x + 1 < n then
                        let nx := x + 1
                        let hgt := grid[nx]![y]!
                        if hgt ≥ lo && hgt ≤ hi then [(nx, y)] else []
                      else []
          let left := if y = 0 then [] else
                        let ny := y - 1
                        let hgt := grid[x]![ny]!
                        if hgt ≥ lo && hgt ≤ hi then [(x, ny)] else []
          let right := if y + 1 < n then
                         let ny := y + 1
                         let hgt := grid[x]![ny]!
                         if hgt ≥ lo && hgt ≤ hi then [(x, ny)] else []
                       else []
          let neigh := up ++ down ++ left ++ right
          loop (qs ++ neigh) vis
    loop [(0,0)] ({} : Std.HashSet Nat)

/-- Check if there exists path with max-min ≤ diff. --/
partial def canReach (grid : Array (Array Nat)) (n minV maxV diff : Nat) : Bool :=
  let rec go (lo : Nat) : Bool :=
    if lo > maxV then
      false
    else
      let hi := lo + diff
      if bfs grid n lo hi then true else go (lo + 1)
  go minV

/-- Binary search minimal difference. --/
partial def solve (grid : Array (Array Nat)) (n minV maxV : Nat) : Nat :=
  let rec bs (lo hi : Nat) : Nat :=
    if lo ≥ hi then lo else
      let mid := (lo + hi) / 2
      if canReach grid n minV maxV mid then
        bs lo mid
      else
        bs (mid+1) hi
  bs 0 (maxV - minV)

def main : IO Unit := do
  let data ← readInts
  let n := data[0]!
  let mut idx := 1
  let mut grid : Array (Array Nat) := Array.replicate n (Array.replicate n 0)
  let mut minV : Nat := 200
  let mut maxV : Nat := 0
  for i in [0:n] do
    let mut row := Array.replicate n 0
    for j in [0:n] do
      let v := data[idx]!
      idx := idx + 1
      row := row.set! j v
      if v < minV then minV := v
      if v > maxV then maxV := v
    grid := grid.set! i row
  let ans := solve grid n minV maxV
  IO.println ans
