/- Solution for SPOJ POLYEQ - Polynomial Equations
https://www.spoj.com/problems/POLYEQ/
-/

import Std
open Std

/-- parse polynomial string into coefficient array a[0..6] --/
def parsePoly (s : String) : Array Float :=
  let s := s.trim.replace " " ""
  let s := s.replace "-" "+-"
  let toks := s.split (· = '+') |>.filter (fun t => t ≠ "") |>.toArray
  Id.run do
    let mut coeffs : Array Float := Array.mkArray 7 0.0
    for t in toks do
      if t.contains 'x' then
        let parts := t.split (· = 'x') |>.toArray
        let c0 := parts.get! 0
        let c0 := if c0.endsWith "*" then c0.dropRight 1 else c0
        let c0 := if c0 = "" then "1" else if c0 = "-" then "-1" else c0
        let coef := Float.ofInt c0.toInt!
        let p1 := parts.getD 1 ""
        let deg :=
          if p1.startsWith "^" then p1.drop 1 |>.toNat! else 1
        coeffs := coeffs.set! deg (coeffs.get! deg + coef)
      else
        let coef := Float.ofInt t.toInt!
        coeffs := coeffs.set! 0 (coeffs.get! 0 + coef)
    return coeffs

/-- highest non-zero degree --/
def degree (cs : Array Float) : Nat :=
  let mut d := 0
  for i in [0:cs.size] do
    if Float.abs (cs.get! i) > 1e-7 then
      d := i
  d

/-- evaluate polynomial by Horner method --/
def evalPoly (cs : Array Float) (x : Float) : Float :=
  let d := degree cs
  let rec loop (i : Nat) (acc : Float) : Float :=
    if i = 0 then acc
    else
      let j := i - 1
      loop j (acc * x + cs.get! j)
  loop d (cs.get! d)

/-- derivative coefficients --/
def derivative (cs : Array Float) : Array Float :=
  let d := degree cs
  Id.run do
    let mut ds : Array Float := Array.mkArray d 0.0
    for i in [1:d+1] do
      ds := ds.set! (i-1) (Float.ofNat i * cs.get! i)
    return ds

/-- bisection on [a,b] assuming f(a)*f(b)<=0 --/
def bisect (cs : Array Float) (a b : Float) : Float :=
  let rec loop (l r fl fr : Float) (it : Nat) : Float :=
    if it = 0 then (l + r) / 2.0
    else
      let m := (l + r) / 2.0
      let fm := evalPoly cs m
      if Float.abs fm < 1e-7 then m
      else if fl * fm ≤ 0 then loop l m fl fm (it-1)
      else loop m r fm fr (it-1)
  loop a b (evalPoly cs a) (evalPoly cs b) 100

/-- divide polynomial by (x - r) using synthetic division --/
def divide (cs : Array Float) (r : Float) : Array Float :=
  let d := degree cs
  Id.run do
    let mut b : Array Float := Array.mkArray (d+1) 0.0
    b := b.set! d (cs.get! d)
    for i in [0:d] do
      let idx := d - 1 - i
      let val := cs.get! idx + r * (b.get! (idx+1))
      b := b.set! idx val
    let mut res : Array Float := Array.mkArray d 0.0
    for i in [0:d] do
      res := res.set! i (b.get! (i+1))
    return res

partial def solvePoly (cs : Array Float) : List Float :=
  let d := degree cs
  if d = 0 then []
  else if d = 1 then
    [ - (cs.get! 0) / (cs.get! 1) ]
  else
    let crit := solvePoly (derivative cs) |>.qsort (· < ·)
    let points := ([-100.0] ++ crit) ++ [100.0]
    let rec findRoot (ps : List Float) : Float :=
      match ps with
      | a :: b :: rest =>
          let fa := evalPoly cs a
          if Float.abs fa < 1e-7 then a
          else
            let fb := evalPoly cs b
            if Float.abs fb < 1e-7 then b
            else if fa * fb > 0 then findRoot (b :: rest)
            else bisect cs a b
      | _ => 0.0
    let r := findRoot points
    r :: solvePoly (divide cs r)

/-- round to two decimals --/
def round2 (x : Float) : Float :=
  let y := x * 100.0
  let y := if y ≥ 0.0 then y + 0.5 else y - 0.5
  (Float.ofInt (Int.ofFloat y)) / 100.0

/-- format with two decimals --/
def fmt (x : Float) : String :=
  let r := round2 x
  let s := toString r
  if s.contains '.' then
    let parts := s.split (· = '.') |>.toArray
    let intPart := parts.get! 0
    let frac := parts.get! 1
    if frac.length = 1 then intPart ++ "." ++ frac ++ "0"
    else if frac.length = 2 then s
    else intPart ++ "." ++ frac.take 2
  else s ++ ".00"

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let cs := parsePoly line.trim
    let roots := solvePoly cs
    let out := String.intercalate " " (roots.map fmt)
    IO.println out
    process h (t-1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let tLine ← stdin.getLine
  let t := tLine.trim.toNat!
  process stdin t
