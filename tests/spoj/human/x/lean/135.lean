/- Solution for SPOJ MAWORK - Men at work
https://www.spoj.com/problems/MAWORK/
-/

import Std
open Std

-- Check whether cell (r,c) is open at time t.
def isOpen (grid : Array (Array Bool)) (per : Array (Array Nat))
    (r c t : Nat) : Bool :=
  let open0 := (grid[r]!)[c]!
  let p := (per[r]!)[c]!
  if p = 0 then
    open0
  else
    let cycle := 2 * p
    let phase := t % cycle
    if open0 then
      decide (phase < p)
    else
      decide (phase ≥ p)

-- Breadth-first search on the time-expanded grid.
partial def bfs (n : Nat) (grid : Array (Array Bool)) (per : Array (Array Nat)) :
    Option Nat := Id.run do
  let mut lcm : Nat := 1
  for i in [0:n] do
    for j in [0:n] do
      let p := (per[i]!)[j]!
      let cycle := if p = 0 then 1 else 2 * p
      lcm := Nat.lcm lcm cycle
  let total := n * n * lcm
  let idx := fun (r c t : Nat) => ((r * n + c) * lcm) + (t % lcm)
  if !isOpen grid per 0 0 0 then
    return none
  let mut visited : Array Bool := Array.replicate total false
  visited := visited.set! (idx 0 0 0) true
  let mut q : Std.Queue (Nat × Nat × Nat) := .empty
  q := q.enqueue (0,0,0)
  let dirs : Array (Int × Int) := #[(1,0),(-1,0),(0,1),(0,-1),(0,0)]
  let nInt := Int.ofNat n
  let mut res : Option Nat := none
  while res.isNone do
    match q.dequeue? with
    | none => break
    | some ((r,c,t), q') =>
        q := q'
        if r = n-1 && c = n-1 then
          res := some t
        else
          let nt := t + 1
          for d in dirs do
            let nr := Int.ofNat r + d.fst
            let nc := Int.ofNat c + d.snd
            if decide (0 ≤ nr) && decide (nr < nInt) &&
               decide (0 ≤ nc) && decide (nc < nInt) then
              let r2 := Int.toNat nr
              let c2 := Int.toNat nc
              if isOpen grid per r2 c2 nt then
                let id := idx r2 c2 nt
                if !visited[id]! then
                  visited := visited.set! id true
                  q := q.enqueue (r2,c2,nt)
  return res

-- Parse input and solve all test cases.
partial def solveAll (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  for _ in [0:t] do
    let n := (← h.getLine).trim.toNat!
    let mut grid : Array (Array Bool) := Array.replicate n (Array.replicate n false)
    for i in [0:n] do
      let line := (← h.getLine).trim
      let arr := line.data.map (fun c => c == '.') |> List.toArray
      grid := grid.set! i arr
    let mut per : Array (Array Nat) := Array.replicate n (Array.replicate n 0)
    for i in [0:n] do
      let line := (← h.getLine).trim
      let arr := line.data.map (fun c => c.toNat - '0'.toNat) |> List.toArray
      per := per.set! i arr
    match bfs n grid per with
    | some v => IO.println s!"{v}"
    | none   => IO.println "NO"

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solveAll h t
