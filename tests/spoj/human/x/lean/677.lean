/- Solution for SPOJ BROW - A place for the brewery
https://www.spoj.com/problems/BROW/
-/

import Std
open Std

private def solveCase (n : Nat) (zs ds : Array Int) : Int :=
  Id.run do
    -- positions of cities
    let mut pos : Array Int := Array.replicate n 0
    let mut cur : Int := 0
    for i in [1:n] do
      cur := cur + ds[i-1]!
      pos := pos.set! i cur
    let L : Int := ds.foldl (fun acc x => acc + x) 0
    let size2 := 2 * n
    -- duplicate arrays to linearize circle
    let mut posExt : Array Int := Array.replicate size2 0
    let mut zExt : Array Int := Array.replicate size2 0
    for i in [0:n] do
      posExt := posExt.set! i pos[i]!
      zExt := zExt.set! i zs[i]!
      posExt := posExt.set! (i + n) (pos[i]! + L)
      zExt := zExt.set! (i + n) zs[i]!
    -- prefix sums of demands and demand*position
    let mut prefZ : Array Int := Array.replicate (size2 + 1) 0
    let mut prefW : Array Int := Array.replicate (size2 + 1) 0
    for i in [0:size2] do
      let z := zExt[i]!
      let p := posExt[i]!
      prefZ := prefZ.set! (i+1) (prefZ[i]! + z)
      prefW := prefW.set! (i+1) (prefW[i]! + z * p)
    let half := L / 2
    let mut r : Nat := 0
    let mut best : Int := Int.ofNat (1 <<< 60)
    for i in [0:n] do
      if r < i then r := i
      while r + 1 < i + n && decide (posExt[r+1]! - posExt[i]! ≤ half) do
        r := r + 1
      let sZ1 := prefZ[r+1]! - prefZ[i+1]!
      let sW1 := prefW[r+1]! - prefW[i+1]!
      let costCW := sW1 - posExt[i]! * sZ1
      let sZ2 := prefZ[i + n]! - prefZ[r+1]!
      let sW2 := prefW[i + n]! - prefW[r+1]!
      let costCCW := (L + posExt[i]!) * sZ2 - sW2
      let total := costCW + costCCW
      if total < best then best := total
    return best

partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    Id.run do
      let n := toks[idx]!.toNat!
      let mut j := idx + 1
      let mut zs : Array Int := Array.replicate n 0
      let mut ds : Array Int := Array.replicate n 0
      for i in [0:n] do
        let z := toks[j]!.toInt!
        let d := toks[j+1]!.toInt!
        zs := zs.set! i z
        ds := ds.set! i d
        j := j + 2
      let ans := solveCase n zs ds
      return process toks j (t-1) (toString ans :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let res := process toks 1 t []
  IO.println (String.intercalate "\n" res)
