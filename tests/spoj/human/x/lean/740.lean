/- Solution for SPOJ TRT - Treats for the Cows
https://www.spoj.com/problems/TRT/
-/

import Std
open Std

/-- compute maximum revenue for given treat values --/
def solve (vals : Array Nat) : Nat :=
  let n := vals.size
  let mut dp : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  -- base: when only one treat remains
  for i in [0:n] do
    let v := vals.get! i
    let row := dp.get! i
    let row := row.set! i (v * n)
    dp := dp.set! i row
  -- consider segments of increasing length
  for len in [2:n+1] do
    let age := n - len + 1
    for i in [0:(n - len + 1)] do
      let j := i + len - 1
      let left := vals.get! i * age + (dp.get! (i+1)).get! j
      let right := vals.get! j * age + (dp.get! i).get! (j-1)
      let row := dp.get! i
      let row := row.set! j (max left right)
      dp := dp.set! i row
  (dp.get! 0).get! (n-1)

/-- main: read input, compute answer, print result --/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
             |>.filter (· ≠ "")
             |> Array.ofList
  let n := toks.get! 0 |>.toNat!
  let mut vals := Array.mkArray n 0
  for i in [0:n] do
    vals := vals.set! i (toks.get! (i+1) |>.toNat!)
  let ans := solve vals
  IO.println ans
