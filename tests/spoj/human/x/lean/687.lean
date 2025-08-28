-- https://www.spoj.com/problems/REPEATS
import Std

open Std

-- compare two substrings of given length
partial def eqSub (s : Array Char) (i j len : Nat) : Bool :=
  let rec loop (k : Nat) : Bool :=
    if k == len then
      true
    else if s.get! (i+k) == s.get! (j+k) then
      loop (k+1)
    else
      false
  loop 0

partial def maxRepeat (s : Array Char) : Nat :=
  let n := s.size
  let mut best : Nat := 1
  for l in [1:n+1] do
    for i in [0:n] do
      if i + l <= n then
        let mut c := 1
        while i + c*l + l <= n && eqSub s (i + (c-1)*l) (i + c*l) l do
          c := c + 1
        if c > best then
          best := c
  best

partial def readNat (h : IO.FS.Handle) : IO Nat := do
  let line ← h.getLine
  pure line.trim.toNat!

partial def readString (h : IO.FS.Handle) (n : Nat) : IO (Array Char) := do
  let mut arr : Array Char := Array.mkEmpty n
  for _ in [0:n] do
    let ch ← h.getLine
    arr := arr.push (ch.trim.get! 0)
  pure arr

partial def solve (h : IO.FS.Handle) : IO Unit := do
  let t ← readNat h
  for _ in [0:t] do
    let n ← readNat h
    let s ← readString h n
    let ans := maxRepeat s
    IO.println (toString ans)

def main : IO Unit := do
  let h ← IO.getStdin
  solve h
