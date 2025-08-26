/- Solution for SPOJ BULK - The Bulk!
https://www.spoj.com/problems/BULK
-/

import Std
open Std

structure Face where
  coord : Int
  poly  : Array (Int × Int)

structure Point where
  x : Int
  y : Int
  z : Int

abbrev Parser := StateM (List Int)

private def next : Parser Int := do
  match (← get) with
  | []      => return 0
  | x :: xs => set xs; return x

private def pointInPoly (poly : Array (Int × Int)) (y z : Int) : Bool :=
  Id.run do
    let mut inside := false
    let mut j := poly.size - 1
    for i in [0:poly.size] do
      let yi := (poly.get! i).fst
      let zi := (poly.get! i).snd
      let yj := (poly.get! j).fst
      let zj := (poly.get! j).snd
      let cond1 := (zi > z) ≠ (zj > z)
      let cond2 :=
        let yF := Float.ofInt y
        let yiF := Float.ofInt yi
        let yjF := Float.ofInt yj
        let zF := Float.ofInt z
        let ziF := Float.ofInt zi
        let zjF := Float.ofInt zj
        yF < (yjF - yiF) * (zF - ziF) / (zjF - ziF) + yiF
      if cond1 && cond2 then
        inside := !inside
      j := i
    return inside

private def dedupSorted (a : Array Int) : Array Int :=
  let sorted := a.qsort (fun x y => x < y)
  Id.run do
    let mut res : Array Int := #[]
    for x in sorted do
      if res.isEmpty || res.back? ≠ some x then
        res := res.push x
    return res

private def solveCase : Parser Nat := do
  let f := (← next).toNat
  let mut xvals : Array Int := #[0, 1001]
  let mut yvals : Array Int := #[0, 1001]
  let mut zvals : Array Int := #[0, 1001]
  let mut faces : Array Face := #[]
  for _ in [0:f] do
    let p := (← next).toNat
    let mut pts : Array Point := Array.mkEmpty p
    for _ in [0:p] do
      let x ← next
      let y ← next
      let z ← next
      pts := pts.push {x := x, y := y, z := z}
      xvals := xvals.push x
      yvals := yvals.push y
      zvals := zvals.push z
    let x0 := (pts.get! 0).x
    let mut allX := true
    for pt in pts do
      if pt.x ≠ x0 then
        allX := false
    if allX then
      let mut poly : Array (Int × Int) := Array.mkEmpty pts.size
      for pt in pts do
        poly := poly.push (pt.y, pt.z)
      faces := faces.push {coord := x0, poly := poly}
  let xs := dedupSorted xvals
  let ys := dedupSorted yvals
  let zs := dedupSorted zvals
  let mut vol : Nat := 0
  for xi in [0:xs.size - 1] do
    let xmid := (xs.get! xi + xs.get! (xi + 1)) / 2
    let dx := (xs.get! (xi + 1) - xs.get! xi).toNat
    for yi in [0:ys.size - 1] do
      let ymid := (ys.get! yi + ys.get! (yi + 1)) / 2
      let dy := (ys.get! (yi + 1) - ys.get! yi).toNat
      for zi in [0:zs.size - 1] do
        let zmid := (zs.get! zi + zs.get! (zi + 1)) / 2
        let dz := (zs.get! (zi + 1) - zs.get! zi).toNat
        let mut cnt : Nat := 0
        for fc in faces do
          if xmid < fc.coord && pointInPoly fc.poly ymid zmid then
            cnt := cnt + 1
        if cnt % 2 == 1 then
          vol := vol + dx * dy * dz
  return vol

private def solveAll : Parser (Array Nat) := do
  let t := (← next).toNat
  let mut res : Array Nat := Array.mkEmpty t
  for _ in [0:t] do
    let v ← solveCase
    res := res.push v
  return res

private def parseInts (s : String) : List Int :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.foldl (fun acc w => if w.isEmpty then acc else Int.ofNat w.toNat! :: acc) []
   |> List.reverse

def main : IO Unit := do
  let data ← IO.getStdin.readToEnd
  let tokens := parseInts data
  let (vals, _) := solveAll.run tokens
  for v in vals do
    IO.println s!"The bulk is composed of {v} units."
