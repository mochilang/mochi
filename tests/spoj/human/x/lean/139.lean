/- Solution for SPOJ MAZE - The Long and Narrow Maze
https://www.spoj.com/problems/MAZE/
-/

import Std
open Std

-- Precomputed transition table for all column types (a,b,c) where
-- each entry is an array of three arrays. For a given start row r,
-- the array lists possible exit rows after traversing the column.
def transTable : Array (Array (Array Nat)) :=
  #[
    -- 0: (0,0,0)
    #[#[], #[], #[]],
    -- 1: (0,0,1)
    #[#[], #[], #[2]],
    -- 2: (0,0,2)
    #[#[], #[], #[]],
    -- 3: (0,1,0)
    #[#[], #[1], #[]],
    -- 4: (0,1,1)
    #[#[], #[1], #[2]],
    -- 5: (0,1,2)
    #[#[], #[1], #[]],
    -- 6: (0,2,0)
    #[#[], #[], #[]],
    -- 7: (0,2,1)
    #[#[], #[], #[2]],
    -- 8: (0,2,2)
    #[#[], #[2], #[1]],
    -- 9: (1,0,0)
    #[#[0], #[], #[]],
    -- 10: (1,0,1)
    #[#[0], #[], #[2]],
    -- 11: (1,0,2)
    #[#[0], #[], #[]],
    -- 12: (1,1,0)
    #[#[0], #[1], #[]],
    -- 13: (1,1,1)
    #[#[0], #[1], #[2]],
    -- 14: (1,1,2)
    #[#[0], #[1], #[]],
    -- 15: (1,2,0)
    #[#[0], #[], #[]],
    -- 16: (1,2,1)
    #[#[0], #[], #[2]],
    -- 17: (1,2,2)
    #[#[0], #[2], #[1]],
    -- 18: (2,0,0)
    #[#[], #[], #[]],
    -- 19: (2,0,1)
    #[#[], #[], #[2]],
    -- 20: (2,0,2)
    #[#[], #[], #[]],
    -- 21: (2,1,0)
    #[#[], #[1], #[]],
    -- 22: (2,1,1)
    #[#[], #[1], #[2]],
    -- 23: (2,1,2)
    #[#[2], #[1], #[0]],
    -- 24: (2,2,0)
    #[#[1], #[0], #[]],
    -- 25: (2,2,1)
    #[#[1], #[0], #[2]],
    -- 26: (2,2,2)
    #[#[1], #[0, 2], #[1]]
  ]

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (toks.get! idx).toNat!
    let mut pos := idx + 1
    let mut reach : Array Bool := #[true, true, true]
    for _ in [0:n] do
      let a := (toks.get! pos).toNat!
      let b := (toks.get! (pos+1)).toNat!
      let c := (toks.get! (pos+2)).toNat!
      pos := pos + 3
      let idx2 := a * 9 + b * 3 + c
      let trans := transTable.get! idx2
      let mut new : Array Bool := #[false, false, false]
      for r in [0:3] do
        if reach.get! r then
          let outs := trans.get! r
          for o in outs do
            new := new.set! o true
      reach := new
    let ans := if (reach.get! 0) || (reach.get! 1) || (reach.get! 2) then
      "yes" else "no"
    IO.println ans
    solve toks pos (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let t := (toks.get! 0).toNat!
  solve toks 1 t
