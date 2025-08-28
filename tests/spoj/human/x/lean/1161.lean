/- Solution for SPOJ TOE1 - Tic-Tac-Toe ( I )
https://www.spoj.com/problems/TOE1/
-/
import Std
open Std

def count (b : Array (Array Char)) (ch : Char) : Nat :=
  b.foldl (fun acc row => row.foldl (fun a c => if c = ch then a + 1 else a) acc) 0

def wins (b : Array (Array Char)) (ch : Char) : Bool :=
  let r0 := b[0]!; let r1 := b[1]!; let r2 := b[2]!
  (r0[0]! = ch && r0[1]! = ch && r0[2]! = ch) ||
  (r1[0]! = ch && r1[1]! = ch && r1[2]! = ch) ||
  (r2[0]! = ch && r2[1]! = ch && r2[2]! = ch) ||
  (r0[0]! = ch && r1[0]! = ch && r2[0]! = ch) ||
  (r0[1]! = ch && r1[1]! = ch && r2[1]! = ch) ||
  (r0[2]! = ch && r1[2]! = ch && r2[2]! = ch) ||
  (r0[0]! = ch && r1[1]! = ch && r2[2]! = ch) ||
  (r0[2]! = ch && r1[1]! = ch && r2[0]! = ch)

def valid (b : Array (Array Char)) : Bool :=
  let cx := count b 'X'
  let co := count b 'O'
  if co > cx || cx > co + 1 then false
  else
    let xwin := wins b 'X'
    let owin := wins b 'O'
    if xwin && owin then false
    else if xwin && cx != co + 1 then false
    else if owin && cx != co then false
    else true

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let l1 := line
      let l2 := (← h.getLine).trim
      let l3 := (← h.getLine).trim
      let board : Array (Array Char) :=
        #[l1.toList.toArray, l2.toList.toArray, l3.toList.toArray]
      let res := if valid board then "yes" else "no"
      IO.println res
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t

