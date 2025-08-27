/- Solution for SPOJ SHAMAN - Shamans
https://www.spoj.com/problems/SHAMAN/
-/

import Std
open Std

abbrev Point := (Int × Int)

/-- angle of point p around center (cx,cy) --/
def angle (cx cy : Float) (p : Point) : Float :=
  Float.atan2 (Float.ofInt p.snd - cy) (Float.ofInt p.fst - cx)

/-- shoelace area of polygon given in order --/
def polyArea (pts : List Point) : Float :=
  let rec loop : List Point → Int → Int
    | [], acc => acc
    | p :: ps, acc =>
      match ps.head? with
      | some q => loop ps (acc + p.fst * q.snd - p.snd * q.fst)
      | none => acc
  let closed := pts ++ [pts.head!]
  Float.ofInt (Int.natAbs (loop closed 0)) / 2.0

/-- area of quadrilateral formed by 4 points --/
def quadArea (a b c d : Point) : Float :=
  let pts := [a,b,c,d]
  let sx := pts.foldl (fun acc p => acc + p.fst) 0
  let sy := pts.foldl (fun acc p => acc + p.snd) 0
  let cx := Float.ofInt sx / 4.0
  let cy := Float.ofInt sy / 4.0
  let sorted := pts.qsort (fun p q => angle cx cy p < angle cx cy q)
  polyArea sorted

/-- brute force over all quadruples --/
def maxQuad (arr : Array Point) : Float := Id.run do
  let n := arr.size
  let mut best := 0.0
  for i in [0:n] do
    for j in [i+1:n] do
      for k in [j+1:n] do
        for l in [k+1:n] do
          let area := quadArea (arr.get! i) (arr.get! j) (arr.get! k) (arr.get! l)
          if area > best then best := area
  return best

/-- format float with one decimal --/
def format1 (x : Float) : String :=
  let r := (Float.round (x * 10.0)) / 10.0
  let s := r.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      (parts.get! 1 ++ "0").take 1
    else
      "0"
  intPart ++ "." ++ fracPart

partial def readPts (toks : Array String) (idx n : Nat) (acc : Array Point) : (Array Point × Nat) :=
  if n = 0 then (acc, idx) else
    let x := toks.get! idx |>.toInt!
    let y := toks.get! (idx+1) |>.toInt!
    readPts toks (idx+2) (n-1) (acc.push (x,y))

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let (pts, idx') := readPts toks (idx+1) n #[]
    let ans := format1 (maxQuad pts)
    solveAll toks idx' (t-1) (ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 t []
  for line in outs do
    IO.println line
