/- Solution for SPOJ TREE1 - Tree
https://www.spoj.com/problems/TREE1/
-/

import Std
open Std

def fact : Nat -> Nat
| 0 => 1
| n+1 => (n+1) * fact n

def choose (n k : Nat) : Nat :=
  fact n / (fact k * fact (n - k))

partial def count (seq : List Nat) : Nat :=
  match seq with
  | [] => 1
  | root :: rest =>
      let left := rest.takeWhile (fun x => x < root)
      let l := left.length
      let right := rest.drop l
      choose rest.length l * count left * count right

partial def process (h : IO.FS.Stream) : Nat -> IO Unit
| 0 => pure ()
| Nat.succ t => do
    let _n := (← h.getLine).trim.toNat!
    let tokens := (← h.getLine).trim.split (· = ' ')
                  |>.filter (fun s => s ≠ "")
                  |>.map (fun s => s.toNat!)
    IO.println (count tokens)
    process h t

def main : IO Unit := do
  let h ← IO.getStdin
  let d := (← h.getLine).trim.toNat!
  process h d
