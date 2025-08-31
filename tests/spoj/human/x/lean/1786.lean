/- Solution for SPOJ DANGER - In Danger
https://www.spoj.com/problems/DANGER/
-/

import Std
open Std

def parseN (s : String) : Nat :=
  let chars := s.data
  let x := (chars.get! 0).toNat - '0'.toNat
  let y := (chars.get! 1).toNat - '0'.toNat
  let z := (chars.get! 3).toNat - '0'.toNat
  ((x * 10) + y) * (10 ^ z)

def highestPow2 (n : Nat) : Nat :=
  let rec aux (p : Nat) :=
    if p * 2 > n then p else aux (p * 2)
  aux 1

def josephus (n : Nat) : Nat :=
  let l := highestPow2 n
  2 * (n - l) + 1

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let s := line.trim
  if s == "00e0" then
    pure ()
  else
    let n := parseN s
    IO.println (josephus n)
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
