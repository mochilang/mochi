/- Solution for SPOJ HEAPULM - Binary Search Heap Construction
https://www.spoj.com/problems/HEAPULM/
-/

import Std
open Std

structure Node where
  label : String
  prio  : Nat

/-- Build treap from given nodes using Cartesian tree construction. -/
def buildTreap (nodes : Array Node) : String :=
  Id.run do
    let nodes := nodes.qsort (fun a b => a.label < b.label)
    let n := nodes.size
    let mut left : Array (Option Nat) := Array.mkArray n none
    let mut right : Array (Option Nat) := Array.mkArray n none
    let mut stack : Array (Nat × Nat) := Array.mkEmpty n
    for i in [0:n] do
      let pr := (nodes.get! i).prio
      let mut last : Option Nat := none
      while !stack.isEmpty && stack.back!.2 < pr do
        let idx := stack.back!.1
        stack := stack.pop!
        last := some idx
      if !stack.isEmpty then
        let idx := stack.back!.1
        right := right.set! idx (some i)
      match last with
      | some idx => left := left.set! i (some idx)
      | none => ()
      stack := stack.push (i, pr)
    let root := stack.get! 0 |>.1
    let rec dfs (i : Nat) : String :=
      let mut s := "("
      match left.get! i with
      | some l => s := s ++ dfs l
      | none => ()
      let node := nodes.get! i
      s := s ++ node.label ++ "/" ++ toString node.prio
      match right.get! i with
      | some r => s := s ++ dfs r
      | none => ()
      s := s ++ ")"
      s
    return dfs root

/-- Read all tokens from stdin. -/
def readTokens : IO (Array String) := do
  let s ← IO.readStdin
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (· ≠ "") |>.toArray

/-- Solve the problem and produce outputs for all test cases. -/
def solve (tokens : Array String) : Array String :=
  Id.run do
    let mut idx := 0
    let mut outs : Array String := #[]
    while idx < tokens.size do
      let n := tokens.get! idx |>.toNat!
      idx := idx + 1
      if n == 0 then
        break
      let mut ns : Array Node := Array.mkEmpty n
      for _ in [0:n] do
        let tok := tokens.get! idx
        idx := idx + 1
        let sp := tok.splitOn "/"
        ns := ns.push { label := sp.get! 0, prio := sp.get! 1 |>.toNat! }
      outs := outs.push (buildTreap ns)
    return outs

@[main] def main : IO Unit := do
  let tokens ← readTokens
  let outs := solve tokens
  IO.println (String.intercalate "\n" outs)
