/- Solution for SPOJ CONDUIT - I Conduit
https://www.spoj.com/problems/CONDUIT/
-/
import Std
open Std

structure Interval where
  s : Int
  e : Int
  deriving Inhabited

def gcdInt (a b : Int) : Int :=
  Nat.gcd a.natAbs b.natAbs

def canonicalDir (dx dy : Int) : Int × Int :=
  let g := gcdInt dx dy
  let dx := dx / g
  let dy := dy / g
  if dx < 0 || (dx == 0 && dy < 0) then
    (-dx, -dy)
  else
    (dx, dy)

def parseCoord (s : String) : Int :=
  let parts := s.splitOn "."
  let whole := parts[0]!
  let frac := if h : 1 < parts.length then parts[1]! else ""
  let frac := (frac ++ "00").take 2
  let w := whole.toInt!
  let f := frac.toNat!
  if w >= 0 then w*100 + f else w*100 - f

def mergeCount (intervals : Array Interval) : Nat :=
  Id.run do
    if intervals.isEmpty then
      return 0
    let arr := intervals.qsort (fun a b => a.s < b.s)
    let mut cur := arr[0]!
    let mut cnt : Nat := 1
    for i in [1:arr.size] do
      let iv := arr[i]!
      if iv.s <= cur.e then
        cur := {s := cur.s, e := max cur.e iv.e}
      else
        cnt := cnt + 1
        cur := iv
    return cnt

def processCase (segs : List (Int×Int×Int×Int)) : Nat :=
  Id.run do
    let mut map : Std.HashMap (Int×Int×Int) (Array Interval) := {}
    for (x1,y1,x2,y2) in segs do
      let dx := x2 - x1
      let dy := y2 - y1
      let (dxn,dyn) := canonicalDir dx dy
      let k := dxn * y1 - dyn * x1
      let t1 := dxn * x1 + dyn * y1
      let t2 := dxn * x2 + dyn * y2
      let key := (dxn,dyn,k)
      let arr := match map.get? key with | some v => v | none => #[]
      let a := min t1 t2
      let b := max t1 t2
      map := map.insert key (arr.push {s := a, e := b})
    let mut total : Nat := 0
    for (_k, arr) in map.toList do
      total := total + mergeCount arr
    return total

partial def loop (toks : Array String) (idx : Nat) (outs : Array String) : Array String :=
  if h : idx < toks.size then
    let n := toks[idx]!.toNat!
    if n == 0 then outs else
      let rec readSegs (j : Nat) (m : Nat) (acc : List (Int×Int×Int×Int)) : (List (Int×Int×Int×Int) × Nat) :=
        if m = 0 then (acc, j) else
          let x1 := parseCoord (toks[j]!)
          let y1 := parseCoord (toks[j+1]!)
          let x2 := parseCoord (toks[j+2]!)
          let y2 := parseCoord (toks[j+3]!)
          readSegs (j+4) (m-1) ((x1,y1,x2,y2)::acc)
      let (segs, j) := readSegs (idx+1) n []
      let ans := processCase segs
      loop toks j (outs.push (toString ans))
  else
    outs

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "") |>.toArray
  let outs := loop toks 0 #[]
  for s in outs do
    IO.println s
