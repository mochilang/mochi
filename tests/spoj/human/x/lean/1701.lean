/- Solution for SPOJ EOWAMRT - Earth Observation with a Mobile Robot Team
https://www.spoj.com/problems/EOWAMRT/
-/

import Std
open Std

structure Segment where
  s e px py vx vy : Float

structure Robot where
  name : String
  segs : Array Segment

partial def parseRobot (toks : Array String) (idx : Nat) (T : Float) : (Robot × Nat) := Id.run do
  let name := toks.get! idx
  let mut i := idx + 1
  let t0 := toks.get! i |>.toFloat!
  i := i + 1
  let x0 := toks.get! i |>.toFloat!
  i := i + 1
  let y0 := toks.get! i |>.toFloat!
  i := i + 1
  let mut segs : Array Segment := #[]
  let mut tp := t0
  let mut px := x0
  let mut py := y0
  while tp < T do
    let t := toks.get! i |>.toFloat!
    i := i + 1
    let vx := toks.get! i |>.toFloat!
    i := i + 1
    let vy := toks.get! i |>.toFloat!
    i := i + 1
    segs := segs.push {s := tp, e := t, px := px, py := py, vx := vx, vy := vy}
    px := px + (t - tp) * vx
    py := py + (t - tp) * vy
    tp := t
  return ({name := name, segs := segs}, i)

private def calcInterval (rx ry vx vy len R : Float) : Option (Float × Float) :=
  let a := vx * vx + vy * vy
  let b := 2.0 * (rx * vx + ry * vy)
  let c := rx * rx + ry * ry - R * R
  if a == 0.0 then
    if rx * rx + ry * ry ≤ R * R then some (0.0, len) else none
  else
    let disc := b * b - 4.0 * a * c
    if disc < 0.0 then
      if rx * rx + ry * ry ≤ R * R then some (0.0, len) else none
    else
      let sqrtD := Float.sqrt disc
      let t1 := (-b - sqrtD) / (2.0 * a)
      let t2 := (-b + sqrtD) / (2.0 * a)
      let start := Float.max 0.0 (Float.min t1 t2)
      let finish := Float.min len (Float.max t1 t2)
      if finish > start then some (start, finish) else none

private def intervalsBetween (a b : Robot) (R : Float) : Array (Float × Float) :=
  Id.run do
    let mut res : Array (Float × Float) := #[]
    let mut ia := 0
    let mut ib := 0
    while ia < a.segs.size && ib < b.segs.size do
      let sa := a.segs.get! ia
      let sb := b.segs.get! ib
      let s := if sa.s > sb.s then sa.s else sb.s
      let e := if sa.e < sb.e then sa.e else sb.e
      if s < e then
        let ax := sa.px + sa.vx * (s - sa.s)
        let ay := sa.py + sa.vy * (s - sa.s)
        let bx := sb.px + sb.vx * (s - sb.s)
        let by := sb.py + sb.vy * (s - sb.s)
        let rx := ax - bx
        let ry := ay - by
        let vx := sa.vx - sb.vx
        let vy := sa.vy - sb.vy
        let len := e - s
        match calcInterval rx ry vx vy len R with
        | some (st, en) =>
            res := res.push (s + st, s + en)
        | none => pure ()
      if sa.e < sb.e then
        ia := ia + 1
      else
        ib := ib + 1
    return res

private def buildAdj (robots : Array Robot) (R : Float) :
    Array (Array (Array (Float × Float))) :=
  Id.run do
    let n := robots.size
    let mut adj := Array.mkArray n (Array.mkArray n #[])
    for i in [0:n] do
      for j in [i+1:n] do
        let ints := intervalsBetween (robots.get! i) (robots.get! j) R
        adj := adj.set! i ((adj.get! i).set! j ints)
        adj := adj.set! j ((adj.get! j).set! i ints)
    return adj

private def propagate (robots : Array Robot)
    (adj : Array (Array (Array (Float × Float)))) (T : Float) : Array String :=
  let n := robots.size
  let INF := 1.0e18
  Id.run do
    let mut dist : Array Float := Array.mkArray n INF
    let mut vis : Array Bool := Array.mkArray n false
    dist := dist.set! 0 0.0
    for _ in [0:n] do
      let mut u := n
      let mut best := INF
      for i in [0:n] do
        if !vis.get! i && dist.get! i < best then
          best := dist.get! i
          u := i
      if u == n then
        pure ()
      else
        vis := vis.set! u true
        let du := dist.get! u
        for j in [0:n] do
          if j ≠ u then
            for inter in (adj.get! u).get! j do
              let s := inter.fst
              let e := inter.snd
              if du ≤ e then
                let cand := if du > s then du else s
                if cand ≤ e && cand < dist.get! j then
                  dist := dist.set! j cand
    let mut names : Array String := #[]
    for i in [0:n] do
      if dist.get! i ≤ T then
        names := names.push (robots.get! i).name
    names := names.qsort (· < ·)
    return names

partial def solveAll (toks : Array String) (idx : Nat) : IO Unit := do
  if idx ≥ toks.size then
    pure ()
  else
    let n := toks.get! idx |>.toNat!
    let T := toks.get! (idx+1) |>.toFloat!
    let R := toks.get! (idx+2) |>.toFloat!
    if n == 0 && T == 0.0 && R == 0.0 then
      pure ()
    else
      let mut i := idx + 3
      let mut robots : Array Robot := #[]
      for _ in [0:n] do
        let (r, j) := parseRobot toks i T
        robots := robots.push r
        i := j
      let adj := buildAdj robots R
      let names := propagate robots adj T
      for name in names do
        IO.println name
      solveAll toks i

private def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.toString.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "")
  return toks.toArray

def main : IO Unit := do
  let toks ← readTokens
  solveAll toks 0
