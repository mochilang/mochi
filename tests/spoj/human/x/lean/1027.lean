/- Solution for SPOJ FPOLICE - Fool the Police
https://www.spoj.com/problems/FPOLICE/
-/

import Std
open Std

private def INF : Nat := 1000000000

-- parse a square matrix of size n from flat array starting at index idx
private def parseMatrix (vals : Array Nat) (idx n : Nat) : (Array (Array Nat) × Nat) :=
  Id.run do
    let mut id := idx
    let mut m : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    for i in [0:n] do
      let mut row : Array Nat := Array.mkArray n 0
      for j in [0:n] do
        row := row.set! j (vals.get! id)
        id := id + 1
      m := m.set! i row
    return (m, id)

private def solveCase (n T : Nat) (time risk : Array (Array Nat)) : (Nat × Nat) :=
  Id.run do
    let mut dp : Array (Array Nat) := Array.mkArray (T+1) (Array.mkArray n INF)
    dp := dp.set! 0 ((dp.get! 0).set! 0 0)
    for t in [0:T+1] do
      for i in [0:n] do
        let cur := (dp.get! t).get! i
        if cur != INF then
          for j in [0:n] do
            let tt := (time.get! i).get! j
            let nt := t + tt
            if nt ≤ T then
              let rr := (risk.get! i).get! j
              let newRisk := cur + rr
              let prev := (dp.get! nt).get! j
              if newRisk < prev then
                dp := dp.set! nt ((dp.get! nt).set! j newRisk)
    let mut bestR := INF
    let mut bestT := 0
    for t in [0:T+1] do
      let r := (dp.get! t).get! (n-1)
      if r < bestR then
        bestR := r
        bestT := t
    if bestR == INF then
      return (INF, INF)
    else
      return (bestR, bestT)

-- process all test cases
partial def process (vals : Array Nat) (idx cases : Nat) (acc : Array String) : Array String :=
  if cases == 0 then acc else
    let n := vals.get! idx
    let T := vals.get! (idx+1)
    let idx := idx + 2
    let (time, idx) := parseMatrix vals idx n
    let (risk, idx) := parseMatrix vals idx n
    let (r, tm) := solveCase n T time risk
    let line := if r == INF then "-1" else s!"{r} {tm}"
    process vals idx (cases - 1) (acc.push line)

-- read all integers from stdin
private def readAll : IO (Array Nat) := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.toString.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "")
  return toks.map (·.toNat!) |>.toArray

def main : IO Unit := do
  let vals ← readAll
  let t := vals.get! 0
  let lines := process vals 1 t #[]
  for s in lines do
    IO.println s
