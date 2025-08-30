/- Solution for SPOJ GRC - Grandpa's Rubik Cube
https://www.spoj.com/problems/GRC/
-/

import Std
open Std

private def rotateFaceCW (c : Array Char) (base : Nat) : Array Char := Id.run do
  let mut c := c
  let a0 := c.get! (base+0)
  let a1 := c.get! (base+1)
  let a2 := c.get! (base+2)
  let a3 := c.get! (base+3)
  let a4 := c.get! (base+4)
  let a5 := c.get! (base+5)
  let a6 := c.get! (base+6)
  let a7 := c.get! (base+7)
  let a8 := c.get! (base+8)
  c := c.set! (base+0) a6
  c := c.set! (base+1) a3
  c := c.set! (base+2) a0
  c := c.set! (base+3) a7
  c := c.set! (base+4) a4
  c := c.set! (base+5) a1
  c := c.set! (base+6) a8
  c := c.set! (base+7) a5
  c := c.set! (base+8) a2
  pure c

private def rotL1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let u0 := c.get! 36
  let u1 := c.get! 39
  let u2 := c.get! 42
  c := c.set! 36 (c.get! 9)
  c := c.set! 39 (c.get! 12)
  c := c.set! 42 (c.get! 15)
  c := c.set! 9 (c.get! 45)
  c := c.set! 12 (c.get! 48)
  c := c.set! 15 (c.get! 51)
  c := c.set! 45 (c.get! 35)
  c := c.set! 48 (c.get! 32)
  c := c.set! 51 (c.get! 29)
  c := c.set! 35 u0
  c := c.set! 32 u1
  c := c.set! 29 u2
  c := rotateFaceCW c 0
  pure c

private def rotR1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let u0 := c.get! 38
  let u1 := c.get! 41
  let u2 := c.get! 44
  c := c.set! 38 (c.get! 33)
  c := c.set! 41 (c.get! 30)
  c := c.set! 44 (c.get! 27)
  c := c.set! 33 (c.get! 53)
  c := c.set! 30 (c.get! 50)
  c := c.set! 27 (c.get! 47)
  c := c.set! 53 (c.get! 11)
  c := c.set! 50 (c.get! 14)
  c := c.set! 47 (c.get! 17)
  c := c.set! 11 u0
  c := c.set! 14 u1
  c := c.set! 17 u2
  c := rotateFaceCW c 18
  pure c

private def rotU1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let b0 := c.get! 27
  let b1 := c.get! 28
  let b2 := c.get! 29
  c := c.set! 27 (c.get! 18)
  c := c.set! 28 (c.get! 19)
  c := c.set! 29 (c.get! 20)
  c := c.set! 18 (c.get! 9)
  c := c.set! 19 (c.get! 10)
  c := c.set! 20 (c.get! 11)
  c := c.set! 9 (c.get! 0)
  c := c.set! 10 (c.get! 1)
  c := c.set! 11 (c.get! 2)
  c := c.set! 0 b0
  c := c.set! 1 b1
  c := c.set! 2 b2
  c := rotateFaceCW c 36
  pure c

private def rotD1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let f0 := c.get! 15
  let f1 := c.get! 16
  let f2 := c.get! 17
  c := c.set! 15 (c.get! 24)
  c := c.set! 16 (c.get! 25)
  c := c.set! 17 (c.get! 26)
  c := c.set! 24 (c.get! 33)
  c := c.set! 25 (c.get! 34)
  c := c.set! 26 (c.get! 35)
  c := c.set! 33 (c.get! 6)
  c := c.set! 34 (c.get! 7)
  c := c.set! 35 (c.get! 8)
  c := c.set! 6 f0
  c := c.set! 7 f1
  c := c.set! 8 f2
  c := rotateFaceCW c 45
  pure c

private def rotF1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let u0 := c.get! 42
  let u1 := c.get! 43
  let u2 := c.get! 44
  c := c.set! 42 (c.get! 8)
  c := c.set! 43 (c.get! 5)
  c := c.set! 44 (c.get! 2)
  c := c.set! 8 (c.get! 47)
  c := c.set! 5 (c.get! 46)
  c := c.set! 2 (c.get! 45)
  c := c.set! 47 (c.get! 18)
  c := c.set! 46 (c.get! 21)
  c := c.set! 45 (c.get! 24)
  c := c.set! 18 u0
  c := c.set! 21 u1
  c := c.set! 24 u2
  c := rotateFaceCW c 9
  pure c

private def rotB1 (c : Array Char) : Array Char := Id.run do
  let mut c := c
  let u0 := c.get! 38
  let u1 := c.get! 37
  let u2 := c.get! 36
  c := c.set! 38 (c.get! 20)
  c := c.set! 37 (c.get! 23)
  c := c.set! 36 (c.get! 26)
  c := c.set! 20 (c.get! 51)
  c := c.set! 23 (c.get! 52)
  c := c.set! 26 (c.get! 53)
  c := c.set! 51 (c.get! 6)
  c := c.set! 52 (c.get! 3)
  c := c.set! 53 (c.get! 0)
  c := c.set! 6 u0
  c := c.set! 3 u1
  c := c.set! 0 u2
  c := rotateFaceCW c 27
  pure c

private def iter (f : Array Char → Array Char) : Nat → Array Char → Array Char
| 0, x => x
| Nat.succ n, x => iter f n (f x)

private def rotL (c : Array Char) (times : Nat) : Array Char :=
  iter rotL1 times c

private def rotR (c : Array Char) (times : Nat) : Array Char :=
  iter rotR1 times c

private def rotU (c : Array Char) (times : Nat) : Array Char :=
  iter rotU1 times c

private def rotD (c : Array Char) (times : Nat) : Array Char :=
  iter rotD1 times c

private def rotF (c : Array Char) (times : Nat) : Array Char :=
  iter rotF1 times c

private def rotB (c : Array Char) (times : Nat) : Array Char :=
  iter rotB1 times c

private def applyMove (c : Array Char) (m : Int) : Array Char :=
  let face := Int.natAbs m
  let times := if m > 0 then 1 else 3
  match face with
  | 1 => rotL c times
  | 2 => rotF c times
  | 3 => rotR c times
  | 4 => rotB c times
  | 5 => rotU c times
  | 6 => rotD c times
  | _ => c

private def isSolved (c : Array Char) : Bool :=
  let checkFace (base : Nat) : Bool :=
    let color := c.get! base
    let mut same := true
    for i in [1:9] do
      same := same && c.get! (base + i) = color
    same
  let mut ok := true
  for f in [0:6] do
    ok := ok && checkFace (f*9)
  ok

private def parseCube (ls : Array String) : Array Char := Id.run do
  let mut cube := Array.mkArray 54 ' '
  -- top
  for i in [0:3] do
    let tokens := (ls.get! i).splitOn " ".filter (· ≠ "")
    for j in [0:3] do
      let ch := (tokens.get! j).data.get! 0
      cube := cube.set! (36 + i*3 + j) ch
  -- middle
  for i in [0:3] do
    let tokens := (ls.get! (i+3)).splitOn " ".filter (· ≠ "")
    for j in [0:12] do
      let ch := (tokens.get! j).data.get! 0
      if j < 3 then
        cube := cube.set! (0 + i*3 + j) ch
      else if j < 6 then
        cube := cube.set! (9 + i*3 + (j-3)) ch
      else if j < 9 then
        cube := cube.set! (18 + i*3 + (j-6)) ch
      else
        cube := cube.set! (27 + i*3 + (j-9)) ch
  -- bottom
  for i in [0:3] do
    let tokens := (ls.get! (i+6)).splitOn " ".filter (· ≠ "")
    for j in [0:3] do
      let ch := (tokens.get! j).data.get! 0
      cube := cube.set! (45 + i*3 + j) ch
  pure cube

private def parseMoves (s : String) : Array Int :=
  ((s.splitOn " ").filter (· ≠ "")).map (fun t => t.toInt!)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let mut lines := Array.mkArray 10 ""
    for i in [0:10] do
      let l := (← h.getLine).trim
      lines := lines.set! i l
    let cube := parseCube lines
    let moves := parseMoves (lines.get! 9)
    let final := moves.foldl applyMove cube
    if isSolved final then
      IO.println "Yes, grandpa!"
    else
      IO.println "No, you are wrong!"
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
