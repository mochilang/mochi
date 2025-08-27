/- Solution for SPOJ VONNY - Vonny and her dominos
https://www.spoj.com/problems/VONNY/
-/

import Std
open Std

/-- index of a domino (unordered pair of numbers 0..6) as 0..27 --/
def pieceIdx (a b : Nat) : Nat :=
  let x := if a ≤ b then a else b
  let y := if a ≤ b then b else a
  6 * x - (x * (x - 1)) / 2 + y

/-- bit mask with bit `n` set --/
def bit (n : Nat) : UInt64 := (1:UInt64) <<< (UInt64.ofNat n)

partial def dfs (board : Array Nat) (pos : Nat) (used pieces : UInt64) : Nat :=
  if pos = 56 then
    1
  else if (used &&& bit pos) ≠ 0 then
    dfs board (pos + 1) used pieces
  else
    let r := pos / 8
    let c := pos % 8
    let v := board[pos]!
    let used1 := used ||| bit pos
    let hcount :=
      if c < 7 then
        let p2 := pos + 1
        if (used &&& bit p2) = 0 then
          let v2 := board[p2]!
          let idx := pieceIdx v v2
          let mask := bit idx
          if (pieces &&& mask) = 0 then
            dfs board (pos + 1) (used1 ||| bit p2) (pieces ||| mask)
          else 0
        else 0
      else 0
    let vcount :=
      if r < 6 then
        let p2 := pos + 8
        if (used &&& bit p2) = 0 then
          let v2 := board[p2]!
          let idx := pieceIdx v v2
          let mask := bit idx
          if (pieces &&& mask) = 0 then
            dfs board (pos + 1) (used1 ||| bit p2) (pieces ||| mask)
          else 0
        else 0
      else 0
    hcount + vcount

partial def readBoard (h : IO.FS.Stream) : IO (Array Nat) := do
  let mut arr : Array Nat := Array.mkEmpty 56
  while arr.size < 56 do
    let line ← h.getLine
    let nums := line.trim.split (· = ' ') |>.filter (· ≠ "")
    for s in nums do
      arr := arr.push (s.toNat!)
  return arr

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let board ← readBoard h
    let ans := dfs board 0 0 0
    IO.println (toString ans)
    solve h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
