/- Solution for SPOJ NSTEPS - Number Steps
https://www.spoj.com/problems/NSTEPS/
-/

import Std
open Std

private def solve (x y : Nat) : Option Nat :=
  if x == y then
    if x % 2 == 0 then some (x + y) else some (x + y - 1)
  else if x == y + 2 then
    if x % 2 == 0 then some (x + y) else some (x + y - 1)
  else
    none

private def parseLine (line : String) : String :=
  let nums := line.split (· = ' ')
               |>.filter (· ≠ "")
               |>.map (fun s => s.toNat!)
  match nums with
  | [x, y] =>
      match solve x y with
      | some n => toString n
      | none   => "No Number"
  | _ => "No Number"

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    IO.println (parseLine line.trim)
    loop h (n - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
