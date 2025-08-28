/- Solution for SPOJ DEADFR - Dead Fraction
https://www.spoj.com/problems/DEADFR/
-/
import Std
open Std

def strToNat (s : String) : Nat :=
  s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

def bestFrac (digits : String) : Nat × Nat :=
  let n := digits.length
  let a := strToNat digits
  let mut bestNum := 0
  let mut bestDen := Nat.succ (10 ^ n)
  for k in List.range n do
    let k := k.succ
    let b := strToNat (digits.take (n - k))
    let num := a - b
    let den := (10 ^ n) - (10 ^ (n - k))
    let g := Nat.gcd num den
    let num := num / g
    let den := den / g
    if den < bestDen then
      bestNum := num
      bestDen := den
  (bestNum, bestDen)

def processLine (line : String) : Option String :=
  if line.trim == "0" then none
  else
    let digits := (line.drop 2).dropRight 3
    let (n, d) := bestFrac digits
    some s!"{n}/{d}"

def main : IO Unit := do
  let rec loop (acc : List String) : IO Unit := do
    let line ← IO.getLine
    match processLine line with
    | none =>
        for s in acc.reverse do
          IO.println s
    | some out =>
        loop (out :: acc)
  loop []
