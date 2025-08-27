/- Solution for SPOJ ONEZERO - Ones and zeros
https://www.spoj.com/problems/ONEZERO/
-/

import Std
open Std

/-- BFS over remainders to find smallest binary-like multiple of `n`. --/
def smallestMultiple (n : Nat) : String := Id.run do
  let r1 := 1 % n
  let mut q : Array (String × Nat) := #[("1", r1)]
  let mut seen : Std.HashSet Nat := Std.HashSet.empty.insert r1
  let mut i : Nat := 0
  while h : i < q.size do
    let (s, r) := q.get ⟨i, h⟩
    if r == 0 then
      return s
    let r0 := (r * 10) % n
    if !seen.contains r0 then
      seen := seen.insert r0
      q := q.push (s ++ "0", r0)
    let r1 := (r * 10 + 1) % n
    if !seen.contains r1 then
      seen := seen.insert r1
      q := q.push (s ++ "1", r1)
    i := i + 1
  return ""

/-- Process test cases. --/
partial def process (arr : Array String) (idx t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (arr.get! idx).toNat!
    IO.println (smallestMultiple n)
    process arr (idx + 1) (t - 1)

/-- Main entry point. --/
def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  process tokens 1 t
