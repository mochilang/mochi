/- Solution for SPOJ MKMONEY - Making Money
https://www.spoj.com/problems/MKMONEY/
-/

import Std
open Std

/-- Read all whitespace-separated tokens from stdin. --/
def readTokens : IO (Array String) := do
  let s ← (← IO.getStdin).readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array String := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p
  return arr

/-- Parse a string like "123.45" into cents as a natural number. --/
def parseCents (s : String) : Nat :=
  match s.splitOn "." with
  | [] => 0
  | [d] => d.toNat! * 100
  | d :: f :: _ =>
      let frac :=
        if f.length = 0 then "00"
        else if f.length = 1 then f ++ "0"
        else f.take 2
      d.toNat! * 100 + frac.toNat!

/-- Parse a percentage like "6.00" into basis points (hundredths of a percent). --/
def parseBasisPoints (s : String) : Nat :=
  match s.splitOn "." with
  | [] => 0
  | [d] => d.toNat! * 100
  | d :: f :: _ =>
      let frac :=
        if f.length = 0 then "00"
        else if f.length = 1 then f ++ "0"
        else f.take 2
      d.toNat! * 100 + frac.toNat!

/-- Format cents as "d.cc" string. --/
def formatCents (x : Nat) : String :=
  let dollars := x / 100
  let cents := x % 100
  let centsStr := if cents < 10 then "0" ++ toString cents else toString cents
  toString dollars ++ "." ++ centsStr

/-- Compute final amount after `c` compounding intervals with rate `bp` (basis points). --/
def compound (p bp c : Nat) : Nat :=
  let denom := 10000 * c
  let rec go (principal : Nat) (n : Nat) : Nat :=
    match n with
    | 0 => principal
    | Nat.succ n' =>
        let interest := principal * bp / denom
        go (principal + interest) n'
  go p c

/-- Main program --/
def main : IO Unit := do
  let toks ← readTokens
  let mut idx : Nat := 0
  let mut caseNum : Nat := 1
  while idx + 2 < toks.size do
    let pStr := toks.get! idx
    let iStr := toks.get! (idx + 1)
    let cStr := toks.get! (idx + 2)
    if pStr = "0.00" && iStr = "0.00" && cStr = "0" then
      break
    let p := parseCents pStr
    let rate := parseBasisPoints iStr
    let c := cStr.toNat!
    let final := compound p rate c
    let line :=
      "Case " ++ toString caseNum ++ ". $" ++ pStr ++ " at " ++ iStr ++ "% APR compounded " ++
      toString c ++ " times yields $" ++ formatCents final
    IO.println line
    idx := idx + 3
    caseNum := caseNum + 1
