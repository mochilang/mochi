/- Solution for SPOJ COVER2 - 3D Cover
https://www.spoj.com/problems/COVER2/
-/

import Std
open Std

structure Cube where
  x1 : Int
  x2 : Int
  y1 : Int
  y2 : Int
  z1 : Int
  z2 : Int

def makeCube (x y z r : Int) : Cube :=
  { x1 := x - r, x2 := x + r,
    y1 := y - r, y2 := y + r,
    z1 := z - r, z2 := z + r }

@[inline] def uniqSort (a : Array Int) : Array Int :=
  let arr := a.qsort (· < ·)
  Id.run do
    let mut res : Array Int := Array.mkEmpty arr.size
    for x in arr do
      if res.size = 0 || res[(res.size - 1)]! ≠ x then
        res := res.push x
    return res

@[inline] def unionVolume (cubes : Array Cube) : Int :=
  Id.run do
    let mut xs : Array Int := Array.mkEmpty (cubes.size * 2)
    let mut ys : Array Int := Array.mkEmpty (cubes.size * 2)
    for c in cubes do
      xs := xs.push c.x1; xs := xs.push c.x2
      ys := ys.push c.y1; ys := ys.push c.y2
    xs := uniqSort xs
    ys := uniqSort ys
    let mut vol : Int := 0
    for xi in [0:xs.size - 1] do
      let xL := xs[xi]!
      let xR := xs[xi + 1]!
      let dx := xR - xL
      if dx ≠ 0 then
        for yi in [0:ys.size - 1] do
          let yL := ys[yi]!
          let yR := ys[yi + 1]!
          let dy := yR - yL
          if dy ≠ 0 then
            let mut segs : Array (Int × Int) := #[]
            for c in cubes do
              if c.x1 ≤ xL && c.x2 ≥ xR && c.y1 ≤ yL && c.y2 ≥ yR then
                segs := segs.push (c.z1, c.z2)
            if segs.size ≠ 0 then
              segs := segs.qsort (fun a b => a.fst < b.fst)
              let mut curL := (segs[0]!).fst
              let mut curR := (segs[0]!).snd
              let mut zSum : Int := 0
              for idx in [1:segs.size] do
                let s := segs[idx]!
                let l := s.fst; let r := s.snd
                if l > curR then
                  zSum := zSum + (curR - curL)
                  curL := l
                  curR := r
                else if r > curR then
                  curR := r
              zSum := zSum + (curR - curL)
              vol := vol + dx * dy * zSum
    return vol

partial def readCubes (toks : Array String) (i cnt : Nat) (acc : Array Cube)
    : (Array Cube × Nat) :=
  if cnt = 0 then
    (acc, i)
  else
    let x := toks[i]!.toInt!
    let y := toks[i+1]!.toInt!
    let z := toks[i+2]!.toInt!
    let r := toks[i+3]!.toInt!
    readCubes toks (i+4) (cnt-1) (acc.push (makeCube x y z r))

partial def solve (toks : Array String) (i cases : Nat) (acc : List String)
    : List String :=
  if cases = 0 then acc.reverse
  else
    let n := toks[i]!.toNat!
    let (cubes, i') := readCubes toks (i+1) n #[]
    let v := unionVolume cubes
    solve toks i' (cases-1) (toString v :: acc)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "") |> List.toArray
  let t := toks[0]!.toNat!
  let outs := solve toks 1 t []
  for line in outs do
    IO.println line
