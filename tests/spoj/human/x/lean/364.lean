/- Solution for SPOJ LISA - Pocket Money
https://www.spoj.com/problems/LISA/
-/

import Std
open Std

/-- Parse the expression into numbers and operators. -/
def parseExpr (s : String) : Array Nat × Array Char := Id.run do
  let mut nums : Array Nat := #[]
  let mut ops : Array Char := #[]
  for c in s.data do
    if c == '+' || c == '*' then
      ops := ops.push c
    else
      nums := nums.push (c.toNat - '0'.toNat)
  pure (nums, ops)

/-- Compute maximum and minimum value of the expression by adding parentheses. -/
def solveExpr (s : String) : Nat × Nat :=
  let (nums, ops) := parseExpr s
  let n := nums.size
  let inf := (Nat.pow 2 63) * 2
  let mut mx : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  let mut mn : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  for i in [:n] do
    let v := nums.get! i
    mx := mx.set! i ((mx.get! i).set! i v)
    mn := mn.set! i ((mn.get! i).set! i v)
  for len in [2:n+1] do
    for i in [0:n-len+1] do
      let j := i + len - 1
      let mut bestMax : Nat := 0
      let mut bestMin : Nat := inf
      for k in [i:j] do
        let op := ops.get! k
        let m1 := (mn.get! i).get! k
        let m2 := (mn.get! (k+1)).get! j
        let M1 := (mx.get! i).get! k
        let M2 := (mx.get! (k+1)).get! j
        let candidateMin := if op == '+' then m1 + m2 else m1 * m2
        let candidateMax := if op == '+' then M1 + M2 else M1 * M2
        if candidateMax > bestMax then
          bestMax := candidateMax
        if candidateMin < bestMin then
          bestMin := candidateMin
      mx := mx.set! i ((mx.get! i).set! j bestMax)
      mn := mn.set! i ((mn.get! i).set! j bestMin)
  ( (mx.get! 0).get! (n-1), (mn.get! 0).get! (n-1) )

/-- Main entry point reading input and printing answers. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let line := (← h.getLine).trim
    let (mx, mn) := solveExpr line
    outs := outs.push s!"{mx} {mn}"
  IO.println (String.intercalate "\n" outs)
