/- Solution for SPOJ HAN01 - Ha-noi!
https://www.spoj.com/problems/HAN01/
-/

import Std
open Std

/-- compute 2^n --/
partial def pow2 : Nat -> Nat
  | 0     => 1
  | n+1 => pow2 n * 2

/-- set positions 1..n to peg p --/
partial def fill (pos : Array Nat) (n p : Nat) : Array Nat :=
  if n == 0 then pos
  else fill (pos.set! n p) (n-1) p

/-- compute positions after k moves of solving Hanoi from src to dst --/
partial def positions (n src dst aux k : Nat) (pos : Array Nat) : Array Nat :=
  if n == 0 || k == 0 then pos
  else
    let half := pow2 (n-1)
    if k < half then
      positions (n-1) src aux dst k pos
    else
      let pos := fill pos (n-1) aux
      let pos := pos.set! n dst
      positions (n-1) aux dst src (k - half) pos

partial def hanoi (n k : Nat) : Array (List Nat) :=
  let pos := Array.mkArray (n+1) 1
  let pos := positions n 1 2 3 k pos
  let mut towers : Array (List Nat) := Array.mkArray 4 []
  for disk in [1:n+1] do
    let p := pos.get! disk
    towers := towers.set! p (disk :: towers.get! p)
  towers

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let ws := line.trim.split (· = ' ')
    let n := ws.get! 0 |>.toNat!
    let k := ws.get! 1 |>.toNat!
    let towers := hanoi n k
    for idx in [1:4] do
      let disks := towers.get! idx
      let s := String.intercalate "|" (disks.map toString)
      if s.isEmpty then
        IO.println s!"{idx}:"
      else
        IO.println s!"{idx}: {s}"
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
