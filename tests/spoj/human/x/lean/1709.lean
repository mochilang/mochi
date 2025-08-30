/- Solution for SPOJ SWTHIN - Swamp Things
https://www.spoj.com/problems/SWTHIN/
-/
import Std
open Std

def slopeKey (dx dy : Int) : Int × Int :=
  let g := Nat.gcd (Int.natAbs dx) (Int.natAbs dy)
  let g := Int.ofNat g
  let dx := dx / g
  let dy := dy / g
  if dx = 0 then (1,0)
  else if dx < 0 then (-dy, -dx) else (dy, dx)

def maxCollinear (pts : Array (Int × Int)) : Nat :=
  Id.run do
    let n := pts.size
    let mut best := if n = 0 then 0 else 1
    for i in [0:n] do
      let (xi, yi) := pts[i]!
      let mut mp : Std.HashMap (Int × Int) Nat := {}
      for j in [i+1:n] do
        let (xj, yj) := pts[j]!
        let key := slopeKey (xj - xi) (yj - yi)
        let old := match Std.HashMap.get? mp key with
          | some v => v
          | none => 0
        let new := old + 1
        mp := mp.insert key new
        if new + 1 > best then
          best := new + 1
    return best

partial def loop (tokens : Array String) (idx photo : Nat) : IO Unit := do
  if idx >= tokens.size then
    pure ()
  else
    let n := (tokens[idx]!).toNat!
    if n = 0 then
      pure ()
    else
      let mut pts : Array (Int × Int) := #[]
      for k in [0:n] do
        let x := (tokens[idx + 1 + 2*k]!).toNat!
        let y := (tokens[idx + 2 + 2*k]!).toNat!
        pts := pts.push (Int.ofNat x, Int.ofNat y)
      let m := maxCollinear pts
      let elim := if m >= 4 then m else 0
      IO.println s!"Photo {photo}: {elim} points eliminated"
      loop tokens (idx + 1 + 2*n) (photo + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokensList := data.split (fun ch => ch = ' ' || ch = '\n' || ch = '\t' || ch = '\r')
                       |> List.filter (fun s => s ≠ "")
  let tokens := tokensList.toArray
  loop tokens 0 1
