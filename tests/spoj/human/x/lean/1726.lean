/- Solution for SPOJ EXCHANGE - Exchange
https://www.spoj.com/problems/EXCHANGE/
-/

import Std
open Std

structure Line where
  m : Float
  c : Float

@[inline] def Line.eval (ln : Line) (x : Float) : Float :=
  ln.m * x + ln.c

structure Node where
  line : Line
  left : Option Node := none
  right : Option Node := none

def parseFloat! (s : String) : Float :=
  match Float.ofString? s with
  | some v => v
  | none => 0.0

def negInf : Float := -1e100

-- Insert a line into the Li Chao tree on interval [l,r]
partial def insertLine : Option Node → Float → Float → Line → Option Node
| none, l, r, ln => some {line := ln}
| some node, l, r, ln =>
  let mid := (l + r) / 2.0
  let cur := node.line
  let (cur, ln) := if Line.eval ln mid > Line.eval cur mid then (ln, cur) else (cur, ln)
  if r - l < 1e-9 then
    some {node with line := cur}
  else if Line.eval ln l > Line.eval cur l then
    let left := insertLine node.left l mid ln
    some {line := cur, left := left, right := node.right}
  else if Line.eval ln r > Line.eval cur r then
    let right := insertLine node.right mid r ln
    some {line := cur, left := node.left, right := right}
  else
    some {node with line := cur}

partial def queryLine : Option Node → Float → Float → Float → Float
| none, _, _, _ => negInf
| some node, l, r, x =>
  let res := Line.eval node.line x
  if r - l < 1e-9 then res
  else
    let mid := (l + r) / 2.0
    if x ≤ mid then
      let q := queryLine node.left l mid x
      if q > res then q else res
    else
      let q := queryLine node.right mid r x
      if q > res then q else res

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

/-- solve single test case -/
def solveCase (n : Nat) (S : Float) (a b r : Array Float) : String :=
  Id.run do
    let mut best : Array Float := Array.replicate n 0.0
    let mut hull : Option Node := none
    let domL : Float := 0.0
    let domR : Float := 100.0
    for k in [0:n] do
      let i := n - 1 - k
      let ri := r[i]!
      let denom := ri * a[i]! + b[i]!
      let y := queryLine hull domL domR ri
      let cand := y / denom
      let besti := if cand > 1.0 then cand else 1.0
      best := best.set! i besti
      let slope := a[i]! * besti
      let inter := b[i]! * besti
      hull := insertLine hull domL domR {m := slope, c := inter}
    let ans := S * best[0]!
    format3 ans

partial def readDays (toks : Array String) (idx n : Nat)
    (a b r : Array Float) : (Array Float × Array Float × Array Float × Nat) :=
  if h : idx < toks.size ∧ n > 0 then
    let ai := parseFloat! (toks[idx]!)
    let bi := parseFloat! (toks[idx+1]!)
    let ri := parseFloat! (toks[idx+2]!)
    readDays toks (idx+3) (n-1) (a.push ai) (b.push bi) (r.push ri)
  else
    (a,b,r,idx)

partial def solveAll (toks : Array String) (idx t : Nat)
    (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := toks[idx]!.toNat!
    let S := parseFloat! (toks[idx+1]!)
    let (a,b,r,j) := readDays toks (idx+2) n Array.empty Array.empty Array.empty
    let out := solveCase n S a b r
    solveAll toks j (t-1) (out :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  for line in outs do
    IO.println line
