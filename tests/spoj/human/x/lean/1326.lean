/- Solution for SPOJ CHASE - A Chase In WonderLand
https://www.spoj.com/problems/CHASE/
-/

import Std.Data.HashMap
open Std

structure Point where
  x : Int
  y : Int

deriving Inhabited

private def normSlope (dx dy : Int) : Int × Int :=
  let g := Int.ofNat (Nat.gcd dx.natAbs dy.natAbs)
  let dx := dx / g
  let dy := dy / g
  if dx < 0 ∨ (dx == 0 ∧ dy < 0) then (-dx, -dy) else (dx, dy)

private def maxCollinear (pts : Array Point) : Nat :=
  if pts.isEmpty then 0 else
  Id.run do
    let n := pts.size
    let mut ans : Nat := 1
    for i in [0:n] do
      let mut mp : Std.HashMap (Int × Int) Nat := {}
      let mut same : Nat := 0
      let pi := pts[i]!
      for j in [i+1:n] do
        let pj := pts[j]!
        let dx := pj.x - pi.x
        let dy := pj.y - pi.y
        if dx == 0 && dy == 0 then
          same := same + 1
        else
          let dir := normSlope dx dy
          let c := Std.HashMap.getD mp dir 0
          mp := Std.HashMap.insert mp dir (c + 1)
      let mut best : Nat := 0
      for (_, v) in mp.toList do
        if v > best then best := v
      if best + same + 1 > ans then
        ans := best + same + 1
    return ans

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokens := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |>.toArray
  let t := (tokens[0]!).toNat!
  let rec loop (idx t : Nat) : IO Unit := do
    if t = 0 then
      pure ()
    else
      let k := (tokens[idx]!).toNat!
      let mut pts : Array Point := #[]
      let mut j := idx + 1
      for _ in [0:k] do
        let x := (tokens[j]!).toInt!
        let y := (tokens[j+1]!).toInt!
        pts := pts.push {x:=x, y:=y}
        j := j + 2
      let m := maxCollinear pts
      IO.println (m - 1)
      loop j (t - 1)
  loop 1 t
