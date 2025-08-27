/- Solution for SPOJ BABTWR - Tower of Babylon
https://www.spoj.com/problems/BABTWR/
-/

import Std
open Std

structure Block where
  w : Nat
  d : Nat
  h : Nat
deriving Repr, Inhabited

/-- generate all rotations of a block, keeping base sides ordered so width ≥ depth -/
def rotations (x y z : Nat) : Array Block :=
  Id.run do
    let mut arr : Array Block := #[]
    if x ≥ y then
      arr := arr.push {w := x, d := y, h := z}
    else
      arr := arr.push {w := y, d := x, h := z}
    if y ≥ z then
      arr := arr.push {w := y, d := z, h := x}
    else
      arr := arr.push {w := z, d := y, h := x}
    if x ≥ z then
      arr := arr.push {w := x, d := z, h := y}
    else
      arr := arr.push {w := z, d := x, h := y}
    return arr

/-- compute maximum tower height from given block orientations -/
def maxHeight (blocks : Array Block) : Nat :=
  Id.run do
    let sorted := blocks.qsort (fun a b => (a.w * a.d) > (b.w * b.d))
    let m := sorted.size
    let mut dp : Array Nat := Array.replicate m 0
    let mut ans : Nat := 0
    for i in [0:m] do
      let b := sorted[i]!
      let mut best := b.h
      for j in [0:i] do
        let p := sorted[j]!
        if p.w > b.w && p.d > b.d then
          best := Nat.max best (dp[j]! + b.h)
      dp := dp.set! i best
      ans := Nat.max ans best
    return ans

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let mut blocks : Array Block := #[]
    for _ in [0:n] do
      let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
      let x := parts[0]! |>.toNat!
      let y := parts[1]! |>.toNat!
      let z := parts[2]! |>.toNat!
      blocks := blocks.append (rotations x y z)
    IO.println (maxHeight blocks)
    loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
