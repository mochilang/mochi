/- Solution for SPOJ ICAMPSEQ - IOICamp Sequence
https://www.spoj.com/problems/ICAMPSEQ/
-/

import Std
open Std

set_option linter.deprecated false
set_option linter.unusedVariables false

private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts[0]!
  let fracPart :=
    if parts.length > 1 then
      let f := parts[1]!
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

private def parseFloat (s : String) : Float :=
  let s := s.trim
  let sign := if s.startsWith "-" then -1.0 else 1.0
  let s := if s.startsWith "-" then s.drop 1 else s
  let parts := s.splitOn "."
  let intPart := parts[0]!
  let intVal := intPart.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let fracPart := if parts.length > 1 then parts[1]! else ""
  let fracVal := fracPart.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let fracLen := fracPart.length
  let denom := Nat.pow 10 fracLen
  sign * (Float.ofNat intVal + Float.ofNat fracVal / Float.ofNat denom)

def updateExt (ext : Array (Float × Float)) (a b c d : Float) :
    Array (Float × Float) :=
  Id.run do
    let coords := #[a, b, c, d]
    let mut e := ext
    for mask in [0:16] do
      let mut s : Float := 0.0
      for j in [0:4] do
        let sgn := if ((mask >>> j) &&& 1) = 1 then 1.0 else -1.0
        s := s + sgn * coords[j]!
      let (mn, mx) := e[mask]!
      let mn := if s < mn then s else mn
      let mx := if s > mx then s else mx
      e := e.set! mask (mn, mx)
    e

partial def parsePoints (toks : Array String) (idx n : Nat)
    (ext : Array (Float × Float)) : Array (Float × Float) :=
  if n = 0 then
    ext
  else
    let a := parseFloat toks[idx]!
    let b := parseFloat toks[idx+1]!
    let c := parseFloat toks[idx+2]!
    let d := parseFloat toks[idx+3]!
    let ext' := updateExt ext a b c d
    parsePoints toks (idx+4) (n-1) ext'

def readInput : IO String := do
  let h ← IO.getStdin
  h.readToEnd

def main : IO Unit := do
  let data ← readInput
  let toks := Array.mk <| data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                        |>.filter (fun s => s ≠ "")
  let n := (toks[0]!).toNat!
  let base := Array.replicate 16 (1e30, -1e30)
  let ext := parsePoints toks 1 n base
  let mut ans := 0.0
  for i in [0:16] do
    let (mn, mx) := ext[i]!
    let diff := mx - mn
    if diff > ans then
      ans := diff
  IO.println (format3 ans)
