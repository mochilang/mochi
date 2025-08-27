/- Solution for SPOJ LITEPIPE - GX Light Pipeline Inc
https://www.spoj.com/problems/LITEPIPE/
-/

import Std
open Std

structure Point where
  x : Float
  y : Float

structure Segment where
  s : Point
  e : Point

def eps : Float := 1e-8

def sgn (x : Float) : Int :=
  if Float.abs x < eps then 0 else if x < 0 then -1 else 1

def cross (a b c : Point) : Float :=
  (a.x - b.x) * (b.y - c.y) - (a.y - b.y) * (b.x - c.x)

def lineSeg (a1 a2 : Point) (seg : Segment) : Bool :=
  sgn (cross a1 seg.s a2) * sgn (cross a1 seg.e a2) ≤ 0

def getIntersect (a b c d : Point) : Point :=
  let A1 := b.y - a.y
  let B1 := a.x - b.x
  let C1 := (b.x - a.x) * a.y - (b.y - a.y) * a.x
  let A2 := d.y - c.y
  let B2 := c.x - d.x
  let C2 := (d.x - c.x) * c.y - (d.y - c.y) * c.x
  let denom := A1 * B2 - A2 * B1
  let x := (C2 * B1 - C1 * B2) / denom
  let y := (C1 * A2 - C2 * A1) / denom
  {x := x, y := y}

def formatFloat (x : Float) : String :=
  let y := Float.floor (x * 100 + 0.5) / 100
  let s := y.toString
  let parts := s.split (· = '.')
  if parts.length = 1 then
    parts[0]! ++ ".00"
  else
    let intPart := parts[0]!
    let fracPart := parts[1]!
    let frac :=
      if fracPart.length ≥ 2 then fracPart.take 2
      else fracPart ++ String.mk (List.replicate (2 - fracPart.length) '0')
    intPart ++ "." ++ frac

def solve (up : Array Point) : Float :=
  Id.run do
    let n := up.size
    let down := up.map (fun p => {x := p.x, y := p.y - 1})
    let mut ans : Float := -1e9
    let mut through := false
    for i in [0:n] do
      if !through then
        for j in [0:n] do
          if i ≠ j && !through then
            let mut flag := true
            let mut k := 0
            while flag && k < n do
              let seg : Segment := {s := up[k]!, e := down[k]!}
              if !lineSeg (up[i]!) (down[j]!) seg then
                flag := false
              else
                k := k + 1
            if flag then
              through := true
              ans := Float.max ans (up[n-1]!.x)
            else if k > Nat.max i j then
              let p1 := getIntersect (up[i]!) (down[j]!) (up[k-1]!) (up[k]!)
              let p2 := getIntersect (up[i]!) (down[j]!) (down[k-1]!) (down[k]!)
              ans := Float.max ans (Float.max p1.x p2.x)
    return if through then up[up.size - 1]!.x else ans

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let t := line.trim
  if t.isEmpty then
    loop h
  else
    let n := t.toNat!
    if n == 0 then
      pure ()
    else
      let mut up : Array Point := #[]
      for _ in [0:n] do
        let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
        let x := parts[0]!.toFloat!
        let y := parts[1]!.toFloat!
        up := up.push {x := x, y := y}
      let ans := solve up
      IO.println (formatFloat ans)
      loop h

def main : IO Unit := do
  loop (← IO.getStdin)
