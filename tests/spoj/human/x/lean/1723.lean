/- Solution for SPOJ BMJ - Bee Maja
https://www.spoj.com/problems/BMJ/
-/

import Std
open Std

-- find minimal ring r such that 1 + 3*r*(r+1) >= n
partial def ring (n : Nat) : Nat :=
  let rec loop (r : Nat) : Nat :=
    if 1 + 3 * r * (r + 1) >= n then r else loop (r + 1)
  loop 0

-- compute coordinates for cell n
def coords (n : Nat) : Int × Int :=
  if n == 1 then (0, 0) else
    let r := ring n
    let rI : Int := Int.ofNat r
    let start : Int := 3 * (rI - 1) * rI + 2
    let offset : Int := Int.ofNat n - start
    let dirs : List (Int × Int) :=
      [(-1,0), (0,-1), (1,-1), (1,0), (0,1), (-1,1)]
    let rec walk (pos : Int × Int) (ds : List (Int × Int)) (left : Int) :=
      match ds with
      | [] => pos
      | (dx,dy) :: rest =>
        if left == 0 then pos
        else
          let step := min left rI
          walk (pos.1 + dx * step, pos.2 + dy * step) rest (left - step)
    walk (0, rI) dirs offset

def main : IO Unit := do
  let content ← IO.getStdin.readToEnd
  for line in content.trim.splitOn "\n" do
    let s := line.trim
    if !s.isEmpty then
      let n := s.toNat!
      let (x, y) := coords n
      IO.println s!"{x} {y}"
