/- Solution for SPOJ RHOMBS - Rhombs
https://www.spoj.com/problems/RHOMBS/
-/
import Std
open Std

def dirs : Array (Int × Int × Int) :=
  #[(1,0,-1),(1,-1,0),(0,-1,1),(-1,0,1),(-1,1,0),(0,1,-1)]

partial def solveCase (toks : Array String) (idx : Nat) : (String × Nat) :=
  Id.run do
    let n := toks[idx]!.toNat!
    let mut idx := idx + 1
    let mut counts : Array Int := Array.replicate 6 0
    let mut x := (0 : Int); let mut y := (0 : Int); let mut z := (0 : Int)
    let mut ar := (0 : Int)
    for _ in [0:n] do
      let d := toks[idx]!.toNat!
      let k := toks[idx+1]!.toNat!
      idx := idx + 2
      counts := counts.modify (d-1) (· + k)
      let (dx,dy,dz) := dirs[d-1]!
      let nx := x + dx * k
      let ny := y + dy * k
      let nz := z + dz * k
      ar := ar + y * nz - ny * z
      x := nx; y := ny; z := nz
    let area : Int := Int.ofNat ar.natAbs
    let n1 := counts[0]! + counts[3]!
    let n2 := counts[1]! + counts[4]!
    let n3 := counts[2]! + counts[5]!
    let a := (area - 2*n1 + n2 + n3) / 6
    let b := (area - 2*n2 + n3 + n1) / 6
    let c := (area - 2*n3 + n1 + n2) / 6
    (s!"{a} {b} {c}", idx)

partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let (res, idx) := solveCase toks idx
    process toks idx (t-1) (res :: acc)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := process toks 1 t []
  for line in outs do
    IO.println line
