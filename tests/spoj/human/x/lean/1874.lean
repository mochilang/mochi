/- Solution for SPOJ BWHEELER - Burrows Wheeler Precompression
https://www.spoj.com/problems/BWHEELER/
-/

import Std
open Std

/-- Convert a lowercase character to an index 0..25. --/
def idx (c : Char) : Nat := c.toNat - 'a'.toNat

/-- Invert the Burrows–Wheeler transform given row `r` and last column `col`. --/
def invBWT (r : Nat) (col : String) : String := Id.run do
  let n := col.length
  let lArr : Array Char := col.data.toArray
  let fArr : Array Char := lArr.qsort (fun a b => a < b)
  -- record positions of each char in first column
  let mut pos : Array (Array Nat) := mkArray 26 #[]
  for j in [0:n] do
    let c := fArr.get! j
    let i := idx c
    pos := pos.set! i ((pos.get! i).push j)
  -- build LF-mapping
  let mut lf : Array Nat := mkArray n 0
  let mut used : Array Nat := mkArray 26 0
  for i in [0:n] do
    let c := lArr.get! i
    let ci := idx c
    let k := used.get! ci
    let j := (pos.get! ci).get! k
    lf := lf.set! i j
    used := used.set! ci (k + 1)
  -- reconstruct string
  let mut row := r - 1
  let mut res : Array Char := mkArray n 'a'
  for k in [0:n] do
    res := res.set! (n - 1 - k) (lArr.get! row)
    row := lf.get! row
  return String.mk res.toList

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let line := line.trim
  if line.isEmpty then
    pure ()
  else
    let r := line.toNat!
    if r == 0 then
      pure ()
    else
      let col ← h.getLine
      let s := invBWT r (col.trim)
      IO.println s
      loop h

def main : IO Unit := do
  loop (← IO.getStdin)
