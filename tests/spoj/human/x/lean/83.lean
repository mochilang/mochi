/- Solution for SPOJ BUNDLE - Bundling
https://www.spoj.com/problems/BUNDLE/
-/

import Std
open Std

structure Template where
  slots : Array Char
  p : Nat

def minPair (a b : Nat × Nat) : Nat × Nat :=
  if a.fst < b.fst then a
  else if b.fst < a.fst then b
  else if a.snd ≤ b.snd then a else b

/-- Solve one test case. -/
def solveCase (temps : Array Template) (types : Array Char) (deps : Array Nat) : Nat × Nat :=
  let n := types.size
  let inf := n + 5
  Id.run do
    let mut dp : Array (Nat × Nat) := Array.mkArray (n+1) (inf, inf)
    dp := dp.set! 0 (0,0)
    for i in [0:n] do
      let (b, s) := dp[i]!
      if b ≠ inf then
        for temp in temps do
          let slots := temp.slots
          let p := temp.p
          for k in [1:4] do
            let j := i + k
            if j ≤ n then
              let mut ok := true
              let mut start := i
              for m in [0:k] do
                if ok then
                  let r := i + m
                  if types[r]! ≠ slots[m]! then
                    ok := false
                  else if deps[r]! ≥ start then
                    ok := false
                  else if p = m+1 ∧ p < k then
                    start := r + 1
              if ok then
                let inner := if p > 0 ∧ p < k then 1 else 0
                let endStop := if j < n then 1 else 0
                let cand := (b + 1, s + inner + endStop)
                let cur := dp[j]!
                dp := dp.set! j (minPair cur cand)
    return dp[n]!

partial def solveAll (toks : Array String) (idx z : Nat) (acc : List String) : List String :=
  if z = 0 then acc.reverse else
    let t := toks[idx]!.toNat!
    let n := toks[idx+1]!.toNat!
    let mut j := idx + 2
    let mut temps := Array.mkArray t { slots := #['A','A','A'], p := 0 }
    for k in [0:t] do
      let letters := toks[j]!
      let p := toks[j+1]!.toNat!
      let slots := #[letters.get! 0, letters.get! 1, letters.get! 2]
      temps := temps.set! k { slots := slots, p := p }
      j := j + 2
    let mut types := Array.mkArray n 'A'
    let mut deps := Array.mkArray n 0
    for k in [0:n] do
      let c := toks[j]!.get! 0
      let d := toks[j+1]!.toNat!
      types := types.set! k c
      deps := deps.set! k d
      j := j + 2
    let (b, s) := solveCase temps types deps
    solveAll toks j (z-1) (s!"{b} {s}" :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let z := toks[0]!.toNat!
  let outs := solveAll toks 1 z []
  IO.println (String.intercalate "\n" outs)
