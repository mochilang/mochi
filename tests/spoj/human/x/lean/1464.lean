/- Solution for SPOJ EDIT3 - Editor II
https://www.spoj.com/problems/EDIT3/
-/
import Std
open Std

partial def readChars (h : IO.FS.Stream) (n : Nat) : IO String := do
  let mut s := ""
  while s.length < n do
    let line ← h.getLine
    s := s ++ line
  return s.take n

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let n := (← h.getLine).trim.toNat!
    let mut text := ""
    let mut pos : Nat := 0
    for _ in [0:n] do
      let ln := (← h.getLine).trim
      if ln.startsWith "Move " then
        pos := (ln.drop 5).toNat!
      else if ln.startsWith "Insert " then
        let len := (ln.drop 7).toNat!
        let s ← readChars h len
        text := text.take pos ++ s ++ text.drop pos
      else if ln.startsWith "Delete " then
        let len := (ln.drop 7).toNat!
        text := text.take pos ++ text.drop (pos + len)
      else if ln.startsWith "Get " then
        let len := (ln.drop 4).toNat!
        IO.println (text.extract pos (pos + len))
      else if ln == "Prev" then
        if pos > 0 then pos := pos - 1
      else if ln == "Next" then
        if pos < text.length then pos := pos + 1
      else
        pure ()
