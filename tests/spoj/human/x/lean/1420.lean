/- Solution for SPOJ GEOM - Geometry and a Square
https://www.spoj.com/problems/GEOM/
-/
import Std
open Std

structure Line where
  a : Float
  b : Float
  c : Float

def intersect (l1 l2 : Line) : Option (Float × Float) :=
  let det := l1.a * l2.b - l2.a * l1.b
  if Float.abs det < 1e-8 then none
  else
    let x := (l1.b * l2.c - l2.b * l1.c) / det
    let y := (l1.c * l2.a - l2.c * l1.a) / det
    some (x, y)

def onLine (l : Line) (p : Float × Float) : Bool :=
  Float.abs (l.a * p.fst + l.b * p.snd + l.c) < 1e-6

/-- round to one decimal place --/
def fmt1 (x : Float) : String :=
  let r := Float.round (x * 10.0) / 10.0
  let s := r.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      ((parts.get! 1) ++ "0").take 1
    else
      "0"
  intPart ++ "." ++ fracPart

partial def process (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  if h : idx + 4 < toks.size then
    let cx := Float.ofInt (toks[idx]!.toInt!)
    let cy := Float.ofInt (toks[idx+1]!.toInt!)
    let s := Float.ofInt (toks[idx+2]!.toInt!)
    let px := Float.ofInt (toks[idx+3]!.toInt!)
    let py := Float.ofInt (toks[idx+4]!.toInt!)
    let hlen := s / 2.0
    let ax := cx - hlen; let ay := cy + hlen
    let bx := cx + hlen; let by := cy + hlen
    let cxv := cx + hlen; let cyv := cy - hlen
    let dx := cx - hlen; let dy := cy - hlen
    let l1 : Line := {a := px - bx, b := py - by, c := -((px - bx)*ax + (py - by)*ay)}
    let l2 : Line := {a := px - cxv, b := py - cyv, c := -((px - cxv)*bx + (py - cyv)*by)}
    let l3 : Line := {a := px - dx, b := py - dy, c := -((px - dx)*cxv + (py - dy)*cyv)}
    let l4 : Line := {a := px - ax, b := py - ay, c := -((px - ax)*dx + (py - ay)*dy)}
    let lines := #[l1,l2,l3,l4]
    let rec search (i j : Nat) : Option (Float × Float) :=
      if i ≥ lines.size then none
      else if j ≥ lines.size then search (i+1) (i+2)
      else
        match intersect (lines.get! i) (lines.get! j) with
        | some p =>
            if lines.all (fun l => onLine l p) then some p
            else search i (j+1)
        | none => search i (j+1)
    match search 0 1 with
    | some (x, y) =>
        let line := s!"YES\n{fmt1 x} {fmt1 y}"
        process toks (idx+5) (line :: acc)
    | none =>
        process toks (idx+5) ("NO" :: acc)
  else
    acc.reverse

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (· ≠ "")
                  |>.toArray
  let outs := process toks 0 []
  for line in outs do
    IO.println line
