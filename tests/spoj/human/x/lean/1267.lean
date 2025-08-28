/- Solution for SPOJ ORIGLIFE - Origin of Life
https://www.spoj.com/problems/ORIGLIFE/
-/

import Std
open Std

-- Precompute neighbor indices for each cell
private def neighborIdx (rows cols : Nat) : Array (List Nat) :=
  Id.run do
    let size := rows * cols
    let mut arr : Array (List Nat) := Array.replicate size []
    for r in [0:rows] do
      for c in [0:cols] do
        let idx := r * cols + c
        let mut lst : List Nat := []
        for dr in [-1,0,1] do
          for dc in [-1,0,1] do
            if dr ≠ 0 || dc ≠ 0 then
              let rr := (Int.ofNat r) + dr
              let cc := (Int.ofNat c) + dc
              if rr >= 0 && rr < rows && cc >= 0 && cc < cols then
                let nidx := rr.toNat * cols + cc.toNat
                lst := nidx :: lst
        arr := arr.set! idx lst
    return arr

-- Build reverse transition mapping
private def buildRev (rows cols a b c : Nat) : Array (List Nat) :=
  Id.run do
    let size := rows * cols
    let total := Nat.pow 2 size
    let neigh := neighborIdx rows cols
    let mut rev : Array (List Nat) := Array.replicate total []
    for s in [0:total] do
      let mut nxt : Nat := 0
      for i in [0:size] do
        let alive := Nat.testBit s i
        let count := (neigh[i]!).foldl (fun acc j => acc + (if Nat.testBit s j then 1 else 0)) 0
        let newAlive := if alive then !(count < a || count > b) else count > c
        if newAlive then
          nxt := nxt + Nat.pow 2 i
      let lst := rev[nxt]!
      rev := rev.set! nxt (s :: lst)
    return rev

-- BFS from target backwards to find smallest distance to a Garden of Eden
private def minSteps (rows cols a b c : Nat) (target : Nat) : Int :=
  Id.run do
    let size := rows * cols
    let total := Nat.pow 2 size
    let rev := buildRev rows cols a b c
    let mut visited : Array Bool := Array.replicate total false
    let mut dist : Array Nat := Array.replicate total 0
    let mut queue : Array Nat := #[target]
    visited := visited.set! target true
    let mut head := 0
    while head < queue.size do
      let s := queue[head]!
      head := head + 1
      let d := dist[s]!
      if (rev[s]!).isEmpty then
        return Int.ofNat d
      for p in rev[s]! do
        if !(visited[p]!) then
          visited := visited.set! p true
          dist := dist.set! p (d + 1)
          queue := queue.push p
    return -1

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let nums := line.trim.split (fun ch => ch = ' ')
                    |>.filter (fun s => s ≠ "")
                    |>.map String.toNat!
                    |>.toArray
    let m := nums[0]!
    let n := nums[1]!
    let a := nums[2]!
    let b := nums[3]!
    let c := nums[4]!
    let mut mask : Nat := 0
    for r in [0:m] do
      let s ← h.getLine
      let row := s.trim.toList.toArray
      for col in [0:n] do
        if row[col]! = '*' then
          mask := mask + Nat.pow 2 (r * n + col)
    let ans := minSteps m n a b c mask
    if ans < 0 then
      IO.println "-1"
    else
      IO.println (toString ans)
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
