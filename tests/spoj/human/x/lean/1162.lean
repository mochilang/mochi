/- Solution for SPOJ TOE2 - Tic-Tac-Toe ( II )
https://www.spoj.com/problems/TOE2/
-/

import Std
open Std

-- check if player c has a winning line
def win (s : String) (c : Char) : Bool :=
  let b := s.data
  let lines : List (List Nat) :=
    [[0,1,2],[3,4,5],[6,7,8],
     [0,3,6],[1,4,7],[2,5,8],
     [0,4,8],[2,4,6]]
  lines.any (fun line => line.all (fun i => b.get! i = c))

-- determine if the board represents a valid final position
def validBoard (s : String) : Bool :=
  let xs := s.data.countp (· = 'X')
  let os := s.data.countp (· = 'O')
  let dots := s.data.countp (· = '.')
  let xwin := win s 'X'
  let owin := win s 'O'
  if os > xs then
    false
  else if xs > os + 1 then
    false
  else if xwin && owin then
    false
  else if xwin && xs ≠ os + 1 then
    false
  else if owin && xs ≠ os then
    false
  else if ¬xwin ∧ ¬owin ∧ dots ≠ 0 then
    false
  else
    true

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let s := line.trim
  if s = "end" then
    pure ()
  else
    let res := if validBoard s then "valid" else "invalid"
    IO.println res
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
