/- Solution for SPOJ GALAXY - A Simple Calculator in the Galaxy
https://www.spoj.com/problems/GALAXY/
-/

import Std
open Std

/-- Evaluate a simple arithmetic expression with +, -, or * between two numbers. -/
def evalExpr (s : String) : Int :=
  match s.splitOn "+" with
  | [a, b] => (String.toInt? a).getD 0 + (String.toInt? b).getD 0
  | _ =>
    match s.splitOn "-" with
    | [a, b] => (String.toInt? a).getD 0 - (String.toInt? b).getD 0
    | _ =>
      match s.splitOn "*" with
      | [a, b] => (String.toInt? a).getD 0 * (String.toInt? b).getD 0
      | _ => 0

/-- Main entry point: read the number of test cases and process each expression. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [:t] do
    let line := (← h.getLine).trim
    IO.println (evalExpr line)
