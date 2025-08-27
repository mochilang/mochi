/- Solution for SPOJ MLAND - Land for Motorways
https://www.spoj.com/problems/MLAND/
-/

import Std
open Std

structure Edge where
  u : Nat
  v : Nat
  a : Int
  b : Int

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, parent') := find parent px
    (r, parent'.set! x r)

def cost (e : Edge) (t : Float) : Float :=
  Float.ofInt e.b + Float.ofInt e.a * t

/-- Build MST at time `t` returning (total cost, sum of slopes). -/
def mst (n : Nat) (edges : Array Edge) (t : Float) : (Float × Int) :=
  Id.run do
    let sorted := edges.qsort (fun a b => cost a t < cost b t)
    let mut parent : Array Nat := Array.range n
    let mut total : Float := 0.0
    let mut slope : Int := 0
    let mut used : Nat := 0
    for e in sorted do
      let (ru, p1) := find parent e.u
      let (rv, p2) := find p1 e.v
      parent := p2
      if ru ≠ rv then
        parent := parent.set! ru rv
        total := total + cost e t
        slope := slope + e.a
        used := used + 1
    return (total, slope)

/-- format float with three decimals -/
private def format3 (x : Float) : String :=
  let absx := if x < 0.0 then -x else x
  if absx < 0.0005 then
    "0.000"
  else
    let y := if x ≥ 0.0 then x + 0.0005 else x - 0.0005
    let s := y.toString
    let parts := s.splitOn "."
    let intPart :=
      match parts with
      | p :: _ => if p == "-0" then "0" else p
      | [] => "0"
    let fracPart :=
      match parts.drop 1 with
      | f :: _ => (f ++ "000").take 3
      | [] => "000"
    intPart ++ "." ++ fracPart

/-- Solve single test case -/
def solveCase (n : Nat) (t1 t2 : Int) (edges : Array Edge) : (Float × Float) :=
  let t1f := Float.ofInt t1
  let t2f := Float.ofInt t2
  let (c1, s1) := mst n edges t1f
  let (c2, s2) := mst n edges t2f
  if s1 <= 0 then
    (t1f, c1)
  else if s2 >= 0 then
    (t2f, c2)
  else
    Id.run do
      let mut lo := t1f
      let mut hi := t2f
      for _ in [0:80] do
        let mid := (lo + hi) / 2.0
        let (_, sm) := mst n edges mid
        if sm > 0 then
          lo := mid
        else
          hi := mid
      let (c, _) := mst n edges hi
      (hi, c)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let nmLine := (← h.getLine).trim
    if nmLine = "" then
      loop h t
    else
      let nm := nmLine.split (· = ' ') |>.filter (· ≠ "")
      let n := nm[0]! |>.toNat!
      let m := nm[1]! |>.toNat!
      let ttLine := (← h.getLine).trim
      let tt := ttLine.split (· = ' ') |>.filter (· ≠ "")
      let t1 := tt[0]! |>.toInt!
      let t2 := tt[1]! |>.toInt!
      let mut edges : Array Edge := Array.replicate m {u := 0, v := 0, a := 0, b := 0}
      for i in [0:m] do
        let line := (← h.getLine).trim
        let ps := line.split (· = ' ') |>.filter (· ≠ "")
        let u := ps[0]! |>.toNat!
        let v := ps[1]! |>.toNat!
        let a := ps[2]! |>.toInt!
        let b := ps[3]! |>.toInt!
        edges := edges.set! i {u:=u, v:=v, a:=a, b:=b}
      let (bestT, bestC) := solveCase n t1 t2 edges
      IO.println (format3 bestT ++ " " ++ format3 bestC)
      loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
