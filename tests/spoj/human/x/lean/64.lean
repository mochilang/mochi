/- Solution for SPOJ PERMUT1 - Permutations
https://www.spoj.com/problems/PERMUT1/
-/

import Std
open Std

def maxN : Nat := 12
def maxK : Nat := 100

def dp : Array (Array Nat) :=
  Id.run do
    let mut dp : Array (Array Nat) := Array.mkArray (maxN + 1) (Array.mkArray (maxK + 1) 0)
    let base := (Array.mkArray (maxK + 1) 0).set! 0 1
    dp := dp.set! 0 base
    for n in [1:maxN+1] do
      let prev := dp.get! (n - 1)
      let mut row := Array.mkArray (maxK + 1) 0
      for k in [0:maxK+1] do
        let lim := min k (n - 1)
        let mut s := 0
        for i in [0:lim+1] do
          s := s + prev.get! (k - i)
        row := row.set! k s
      dp := dp.set! n row
    return dp

def countInv (n k : Nat) : Nat :=
  (dp.get! n).get! k

private def solveLine (line : String) : String :=
  let nums := line.split (fun c => c = ' ')
               |>.filter (fun t => t ≠ "")
               |>.map (fun t => t.toNat!)
  match nums with
  | [n, k] => toString (countInv n k)
  | _      => "0"

partial def loop (h : IO.FS.Stream) (d : Nat) : IO Unit := do
  if d = 0 then
    pure ()
  else
    let line ← h.getLine
    IO.println (solveLine line.trim)
    loop h (d - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let d := (← h.getLine).trim.toNat!
  loop h d
