/- Solution for SPOJ MWORDS - Matrix Words
https://www.spoj.com/problems/MWORDS/
-/

import Std
open Std

def calcUpper (n : Nat) : Array (Array Nat) := Id.run do
  let mut dp : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  for i in [0:n] do
    let r := n - 1 - i
    for j in [0:n] do
      let c := n - 1 - j
      let val :=
        if r == n - 1 && c == n - 1 then
          1
        else
          let mut s := 0
          if c + 1 < n && r ≤ c + 1 then
            s := s + (dp.get! r).get! (c + 1)
          if r + 1 < n && r + 1 ≤ c then
            s := s + (dp.get! (r + 1)).get! c
          s
      let row := (dp.get! r).set! c val
      dp := dp.set! r row
  return dp

def calcLower (n : Nat) : Array (Array Nat) := Id.run do
  let mut dp : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  for i in [0:n] do
    let r := n - 1 - i
    for j in [0:n] do
      let c := n - 1 - j
      let val :=
        if r == n - 1 && c == n - 1 then
          1
        else
          let mut s := 0
          if r + 1 < n then
            s := s + (dp.get! (r + 1)).get! c
          if c + 1 < n && r ≥ c + 1 then
            s := s + (dp.get! r).get! (c + 1)
          s
      let row := (dp.get! r).set! c val
      dp := dp.set! r row
  return dp

inductive Part where
  | upper
  | lower

partial def solveCase (n : Nat) (I : Nat) (grid : Array (Array Char)) : String :=
  let up := calcUpper n
  let low := calcLower n
  let total := (up.get! 0).get! 0 + (low.get! 0).get! 0
  let k := I % total
  let start := (grid.get! 0).get! 0
  let rec build (r c : Nat) (part? : Option Part) (k : Nat) (acc : List Char) : List Char :=
    if r == n - 1 && c == n - 1 then
      acc.reverse
    else
      let moves :=
        match part? with
        | none =>
          let m1 := if c + 1 < n then [((grid.get! r).get! (c + 1), (up.get! r).get! (c + 1), r, c + 1, Part.upper)] else []
          let m2 := if r + 1 < n then [((grid.get! (r + 1)).get! c, (low.get! (r + 1)).get! c, r + 1, c, Part.lower)] else []
          m1 ++ m2
        | some Part.upper =>
          let m1 := if c + 1 < n then [((grid.get! r).get! (c + 1), (up.get! r).get! (c + 1), r, c + 1, Part.upper)] else []
          let m2 := if r + 1 < n && r + 1 ≤ c then [((grid.get! (r + 1)).get! c, (up.get! (r + 1)).get! c, r + 1, c, Part.upper)] else []
          m1 ++ m2
        | some Part.lower =>
          let m1 := if r + 1 < n then [((grid.get! (r + 1)).get! c, (low.get! (r + 1)).get! c, r + 1, c, Part.lower)] else []
          let m2 := if c + 1 < n && r ≥ c + 1 then [((grid.get! r).get! (c + 1), (low.get! r).get! (c + 1), r, c + 1, Part.lower)] else []
          m1 ++ m2
      let moves := moves.qsort (fun a b => a.1 < b.1)
      let rec choose (ms : List (Char × Nat × Nat × Nat × Part)) (k : Nat) :=
        match ms with
        | [] => (' ', 0, 0, Part.upper, 0)
        | (ch, cnt, r', c', p) :: rest =>
          if k < cnt then (ch, r', c', p, k) else choose rest (k - cnt)
      let (ch, r', c', p, k') := choose moves k
      build r' c' (some p) k' (ch :: acc)
  let chars := build 0 0 none k [start]
  String.mk chars

partial def process (h : IO.FS.Stream) (case t : Nat) : IO Unit := do
  if case > t then
    pure ()
  else
    let parts := (← h.getLine).trim.split (· = ' ')
    let n := parts.get! 0 |>.toNat!
    let I := parts.get! 1 |>.toNat!
    let mut grid : Array (Array Char) := Array.mkArray n (Array.mkArray n 'A')
    for r in [0:n] do
      let row := (← h.getLine).trim
      grid := grid.set! r row.data.toArray
    IO.println (solveCase n I grid)
    process h (case + 1) t

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h 1 t
