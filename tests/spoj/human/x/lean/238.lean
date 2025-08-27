/- Solution for SPOJ HOLIDAY2 - Getting Rid of the Holidays (Act II)
https://www.spoj.com/problems/HOLIDAY2/
-/

import Std
open Std

/-- read all integers from stdin --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- count how many numbers in `arr` are divisible by `g` --/
def countDiv (arr : Array Nat) (g : Nat) : Nat :=
  arr.foldl (init := 0) (fun acc x => if x % g == 0 then acc + 1 else acc)

def main : IO Unit := do
  let data ← readInts
  let mut idx := 0
  let t := data.get! idx; idx := idx + 1
  for _ in [0:t] do
    let n := data.get! idx; idx := idx + 1
    let k := data.get! idx; idx := idx + 1
    let mut a : Array Nat := Array.mkArray n 0
    for i in [0:n] do
      a := a.set! i (data.get! idx); idx := idx + 1
    let need := n - k
    let mut best : Nat := 1
    -- single-element candidates
    for i in [0:n] do
      let g := a.get! i
      if g > best then
        let c := countDiv a g
        if c >= need then best := g
    -- pair candidates
    for i in [0:n] do
      for j in [i+1:n] do
        let g := Nat.gcd (a.get! i) (a.get! j)
        if g > best then
          let c := countDiv a g
          if c >= need then best := g
    -- determine which indices to remove
    let mut keep : Nat := 0
    let mut rem : Array Nat := #[]
    for i in [0:n] do
      if (a.get! i) % best == 0 && keep < need then
        keep := keep + 1
      else
        rem := rem.push (i+1)
    let out := String.intercalate " " (rem.map (·.toString) |>.toList)
    IO.println out
