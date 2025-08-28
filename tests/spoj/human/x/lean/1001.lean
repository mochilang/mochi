/- Solution for SPOJ BROUL - Babylonian Roulette
https://www.spoj.com/problems/BROUL/
-/

import Std
open Std

/-- compute minimal number of players, or none if impossible -/
def minimalPlayers (p0 bet pf : Nat) : Option Nat :=
  let r0 := p0 % bet
  if pf % bet ≠ r0 then
    none
  else
    let n0 := (p0 - r0) / bet
    let nf := (pf - r0) / bet
    if pf < bet then
      some ((n0 + 2) / 3)
    else
      let diff := if nf ≥ n0 then nf - n0 else n0 - nf
      some ((diff + 2) / 3)

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let trimmed := line.trim
  if trimmed = "" then
    loop h
  else
    let parts := trimmed.split (· = ' ') |>.filter (· ≠ "")
    let p0 := parts[0]! |>.toNat!
    let bet := parts[1]! |>.toNat!
    let pf := parts[2]! |>.toNat!
    if p0 = 0 ∧ bet = 0 ∧ pf = 0 then
      pure ()
    else
      match minimalPlayers p0 bet pf with
      | none => IO.println "No accounting tablet"
      | some ans => IO.println ans
      loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
