/- Solution for SPOJ JEWELS - Jewelry and Fashion
https://www.spoj.com/problems/JEWELS/
-/

import Std
open Std

/-- Longest common subsequence length between two arrays of characters. --/
private def lcs (a b : Array Char) : Nat :=
  Id.run do
    let m := b.size
    let mut prev := Array.replicate (m+1) 0
    let mut curr := Array.replicate (m+1) 0
    for x in a do
      for j in [0:m] do
        let v :=
          if x = b.get! j then
            prev.get! j + 1
          else
            Nat.max (prev.get! (j+1)) (curr.get! j)
        curr := curr.set! (j+1) v
      prev := curr
      curr := Array.replicate (m+1) 0
    return prev.get! m

/-- compute best cut position and total earring length --/
private def bestCut (s : String) : Nat × Nat :=
  let arr := s.toList.toArray
  let n := arr.size
  let mut bestLen := 0
  let mut bestPos := 1
  for k in [1:n] do
    let left := arr.extract 0 k
    let right := arr.extract k n
    let l1 := lcs left right
    let l2 := lcs left (right.toList.reverse.toArray)
    let l := Nat.max l1 l2
    if l > bestLen then
      bestLen := l
      bestPos := k
  (bestLen * 2, bestPos)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let (len, cut) := bestCut line.trim
    IO.println s!"{len} {cut}"
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
