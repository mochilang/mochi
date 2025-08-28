/- Solution for SPOJ HANOI07 - Building the Tower
https://www.spoj.com/problems/HANOI07/
-/

import Std
open Std

structure Key where
  lvl : Nat
  prev : Nat
  sum  : Nat
  deriving BEq, Hashable

partial def ways (H N lvl prev sum : Nat)
    (memo : Std.HashMap Key Nat) : (Nat × Std.HashMap Key Nat) :=
  if sum > N then
    (0, memo)
  else if lvl == H then
    (1, memo)
  else
    match memo.find? {lvl := lvl, prev := prev, sum := sum} with
    | some v => (v, memo)
    | none =>
      let mut memo := memo
      let mut total : Nat := 0
      if prev > 1 then
        let (v1, memo1) := ways H N (lvl+1) (prev-1) (sum + prev - 1) memo
        total := total + v1
        memo := memo1
      if sum + prev + 1 ≤ N then
        let (v2, memo2) := ways H N (lvl+1) (prev+1) (sum + prev + 1) memo
        total := total + v2
        memo := memo2
      let memo := memo.insert {lvl := lvl, prev := prev, sum := sum} total
      (total, memo)

partial def solve (N H M : Nat) : Nat :=
  let (ans, _) := ways H N 1 M M ({} : Std.HashMap Key Nat)
  ans

partial def process (data : Array Nat) (idx t : Nat) (acc : List String) : List String :=
  if t == 0 then acc.reverse
  else
    let N := data[idx]!
    let H := data[idx+1]!
    let M := data[idx+2]!
    let res := solve N H M
    process data (idx+3) (t-1) ((toString res) :: acc)

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

def main : IO Unit := do
  let data ← readInts
  if h : data.size > 0 then
    let t := data[0]!
    let outs := process data 1 t []
    for line in outs do
      IO.println line
  else
    pure ()
