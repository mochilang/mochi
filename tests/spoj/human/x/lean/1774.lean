/- Solution for SPOJ ALL - All Discs Considered
https://www.spoj.com/problems/ALL/
-/

import Std
open Std

/-- simulate installation starting with disc `start` (0 for first DVD, 1 for second).
Returns number of switches between DVDs (not counting initial insert and final removal). -/
def simulate (start n1 n2 : Nat) (adj : Array (List Nat)) (indeg0 : Array Nat) : Nat :=
  Id.run do
    let n := n1 + n2
    let mut indeg := indeg0
    let mut q1 : Array Nat := #[]
    let mut q2 : Array Nat := #[]
    -- collect initial zero-indegree packages
    for i in [1:n+1] do
      if indeg.get! i == 0 then
        if i ≤ n1 then
          q1 := q1.push i
        else
          q2 := q2.push i
    let mut f1 := 0
    let mut f2 := 0
    let mut cur := start
    let mut switches := 0
    let mut processed := 0
    while processed < n do
      if cur == 0 then
        if f1 == q1.size then
          if f2 == q2.size then
            processed := n -- all done
          else
            cur := 1
            switches := switches + 1
        else
          let pkg := q1.get! f1
          f1 := f1 + 1
          processed := processed + 1
          for v in adj.get! pkg do
            let indegv := indeg.get! v
            indeg := indeg.set! v (indegv - 1)
            if indegv == 1 then
              if v ≤ n1 then
                q1 := q1.push v
              else
                q2 := q2.push v
      else
        if f2 == q2.size then
          if f1 == q1.size then
            processed := n
          else
            cur := 0
            switches := switches + 1
        else
          let pkg := q2.get! f2
          f2 := f2 + 1
          processed := processed + 1
          for v in adj.get! pkg do
            let indegv := indeg.get! v
            indeg := indeg.set! v (indegv - 1)
            if indegv == 1 then
              if v ≤ n1 then
                q1 := q1.push v
              else
                q2 := q2.push v
    return switches

/-- process all test cases from token array starting at index `idx` --/
partial def solve (toks : Array Nat) (idx : Nat) (acc : List Nat) : List Nat :=
  if idx ≥ toks.size then acc.reverse else
  let n1 := toks[idx]!
  let n2 := toks[idx+1]!
  let d  := toks[idx+2]!
  if n1 == 0 ∧ n2 == 0 ∧ d == 0 then acc.reverse else
    let n := n1 + n2
    let mut adj : Array (List Nat) := Array.mkArray (n+1) []
    let mut indeg : Array Nat := Array.mkArray (n+1) 0
    let mut i := idx + 3
    for _ in [0:d] do
      let x := toks[i]!
      let y := toks[i+1]!
      i := i + 2
      adj := adj.set! y (x :: adj.get! y)
      indeg := indeg.set! x (indeg.get! x + 1)
    let s1 := simulate 0 n1 n2 adj indeg
    let s2 := simulate 1 n1 n2 adj indeg
    let ans := Nat.min (s1 + 2) (s2 + 2)
    solve toks i (ans :: acc)

/-- entry point --/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
              |>.filter (fun s => s ≠ "")
              |>.map (fun s => s.toNat!)
              |>.toArray
  let res := solve toks 0 []
  let outLines := res.map (fun n => toString n)
  IO.println (String.intercalate "\n" outLines)
