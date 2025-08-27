/- Solution for SPOJ HELPR2D2 - Help R2-D2!
https://www.spoj.com/problems/HELPR2D2/
-/
import Std
open Std

/-- size of segment tree leaves, power of two ≥ 100000 -/
def segSize : Nat := 131072

/-- update index `pos` to `val` in the segment tree `t` -/
partial def segUpdate (t : Array Nat) (pos val : Nat) : Array Nat := Id.run do
  let mut tree := t
  let mut i := pos + segSize
  tree := tree.set! i val
  i := i / 2
  while i > 0 do
    let left := tree.get! (i*2)
    let right := tree.get! (i*2+1)
    tree := tree.set! i (Nat.max left right)
    i := i / 2
  return tree

/-- find the smallest index with remaining capacity ≥ `v`. Returns `segSize` if none. -/
partial def segQuery (tree : Array Nat) (v : Nat) : Nat :=
  if tree.get! 1 < v then
    segSize
  else
    Id.run do
      let mut i := 1
      let mut l := 0
      let mut r := segSize
      while l + 1 < r do
        let mid := (l + r) / 2
        let left := tree.get! (i*2)
        if left >= v then
          i := i*2; r := mid
        else
          i := i*2 + 1; l := mid
      return l

/-- process one test case starting at token index `idx` -/
partial def solveCase (k n : Nat) (toks : Array String) (idx : Nat)
    : (Nat × Nat × Nat) := Id.run do
  let mut tree := Array.mkArray (2*segSize) 0
  let mut cap := Array.mkArray segSize 0
  let mut ships : Nat := 0
  let mut i := idx
  let mut processed : Nat := 0
  while processed < n do
    let tok := toks.get! i; i := i + 1
    if tok = "b" then
      let r := (toks.get! i).toNat!; i := i + 1
      let v := (toks.get! i).toNat!; i := i + 1
      for _ in [0:r] do
        let idx := segQuery tree v
        if idx = segSize then
          let rem := k - v
          cap := cap.set! ships rem
          tree := segUpdate tree ships rem
          ships := ships + 1
        else
          let rem := cap.get! idx - v
          cap := cap.set! idx rem
          tree := segUpdate tree idx rem
        processed := processed + 1
    else
      let v := tok.toNat!
      let idx := segQuery tree v
      if idx = segSize then
        let rem := k - v
        cap := cap.set! ships rem
        tree := segUpdate tree ships rem
        ships := ships + 1
      else
        let rem := cap.get! idx - v
        cap := cap.set! idx rem
        tree := segUpdate tree idx rem
      processed := processed + 1
  let mut waste := 0
  for j in [0:ships] do
    waste := waste + cap.get! j
  return (ships, waste, i)

/-- parse all cases from tokens starting at index 1 -/
partial def readCases (toks : Array String) (idx : Nat) (t : Nat)
    (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let k := (toks.get! idx).toNat!
    let n := (toks.get! (idx+1)).toNat!
    let (s, w, nxt) := solveCase k n toks (idx+2)
    let line := (toString s) ++ " " ++ (toString w)
    readCases toks nxt (t-1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  if toks.size = 0 then return ()
  let t := (toks.get! 0).toNat!
  let outs := readCases toks 1 t []
  for line in outs do
    IO.println line
