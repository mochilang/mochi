/- Solution for SPOJ FIRM - Goods
https://www.spoj.com/problems/FIRM/
-/

import Std
open Std

partial def pairAll : List Nat -> List (Nat × Nat) × List Nat
| a :: b :: rest =>
  let (ps, nxt) := pairAll rest
  ((a,b) :: ps, b :: nxt)
| [a] => ([], [a])
| [] => ([], [])

partial def cycleSteps : List Nat -> List (List (Nat × Nat))
| [] => []
| [_] => []
| lst =>
  let (day, nxt) := pairAll lst
  day :: cycleSteps nxt

partial def mergeTwo : List (List (Nat × Nat)) -> List (List (Nat × Nat)) -> List (List (Nat × Nat))
| [], b => b
| a, [] => a
| a1 :: aRest, b1 :: bRest => (a1 ++ b1) :: mergeTwo aRest bRest

partial def mergeAll : List (List (List (Nat × Nat))) -> List (List (Nat × Nat))
| [] => []
| s :: ss => mergeTwo s (mergeAll ss)

partial def buildCycles (n : Nat) (want : Array Nat) : List (List Nat) :=
  Id.run do
    let mut vis : Array Bool := Array.replicate n false
    let mut res : List (List Nat) := []
    for i in [0:n] do
      if vis[i]! == false then
        let mut cur := i
        let mut cyc : List Nat := []
        while vis[cur]! == false do
          vis := vis.set! cur true
          cyc := (cur+1) :: cyc
          cur := want[cur]!
        if cyc.length > 1 then
          res := cyc.reverse :: res
    return res

def readInput : IO (Nat × Array Nat) := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := (data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                  |>.filter (fun s => s ≠ "")).toArray
  let n := toks[0]!.toNat!
  let mut want : Array Nat := Array.replicate n 0
  for i in [0:n] do
    want := want.set! i (toks[i+1]!.toNat! - 1)
  return (n, want)

def main : IO Unit := do
  let (n, want) ← readInput
  let cycles := buildCycles n want
  let schedules := cycles.map cycleSteps
  let days := mergeAll schedules
  IO.println days.length
  for day in days do
    let pairStrs := day.map (fun (a,b) => s!"{a}-{b}")
    let line := s!"{day.length} " ++ String.intercalate " " pairStrs
    IO.println line
