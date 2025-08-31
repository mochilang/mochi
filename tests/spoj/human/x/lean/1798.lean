/- Solution for SPOJ LUCKYNUM - Lucky Numbers
https://www.spoj.com/problems/LUCKYNUM/
-/

import Std
open Std

-- remove every n-th element from the list
partial def removeEveryNth (l : List Nat) (n : Nat) : List Nat :=
  let rec go (xs : List Nat) (c : Nat) : List Nat :=
    match xs with
    | [] => []
    | x :: xs' =>
        if c + 1 = n then
          go xs' 0
        else
          x :: go xs' (c + 1)
  go l 0

-- generate first `limit` lucky numbers
partial def genLucky (limit maxVal : Nat) : List Nat :=
  let init := (List.range (maxVal - 1)).map (fun i => i + 2)
  let rec loop (seq : List Nat) (acc : List Nat) (k : Nat) : List Nat :=
    if k = limit then acc.reverse
    else
      match seq with
      | [] => acc.reverse
      | p :: rest =>
          let rest' := removeEveryNth rest p
          loop rest' (p :: acc) (k + 1)
  loop init [] 0

-- precompute the first 3000 lucky numbers
private def luckyArr : Array Nat := (genLucky 3000 40000).toArray

-- get the n-th lucky number (1-indexed)
def nthLucky (n : Nat) : Nat := luckyArr.get! (n - 1)

partial def process : IO Unit := do
  let line ← IO.getStdin.getLine
  let n := line.trim.toNat!
  if n = 0 then
    pure ()
  else
    IO.println (toString (nthLucky n))
    process

def main : IO Unit :=
  process
