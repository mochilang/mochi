/- Solution for SPOJ AEROLITE - The Secret of an Aerolite
https://www.spoj.com/problems/AEROLITE/
-/

import Std
open Std

namespace SPOJ1459

abbrev Key := Nat × Nat × Nat × Nat

def MOD : Nat := 11380

partial def count (a b c d : Nat) : StateM (Std.HashMap Key Nat) Nat := do
  let memo ← get
  if let some v := memo.get? (a,b,c,d) then
    return v
  let v ←
    if a == 0 && b == 0 && c == 0 then
      pure 1
    else if d == 0 then
      pure 0
    else
      let mut sum := 0
      -- curly brackets
      for ai in [0:a] do
        for bi in [0:b.succ] do
          for ci in [0:c.succ] do
            if ai < a then
              let inside ← count ai bi ci (d-1)
              let outside ← count (a-1-ai) (b-bi) (c-ci) d
              sum := (sum + inside * outside) % MOD
      -- square brackets (no curly inside)
      for bi in [0:b] do
        for ci in [0:c.succ] do
          if bi < b then
            let inside ← count 0 bi ci (d-1)
            let outside ← count a (b-1-bi) (c-ci) d
            sum := (sum + inside * outside) % MOD
      -- round brackets (only round inside)
      for ci in [0:c] do
        if ci < c then
          let inside ← count 0 0 ci (d-1)
          let outside ← count a b (c-1-ci) d
          sum := (sum + inside * outside) % MOD
      pure sum
  modify (·.insert (a,b,c,d) v)
  return v

def runCount (a b c d : Nat) : Nat :=
  ((count a b c d).run ({} : Std.HashMap Key Nat)).1

def solveCase (a b c d : Nat) : Nat :=
  let total := runCount a b c d
  let prev := if d == 0 then 0 else runCount a b c (d-1)
  (total + MOD - prev) % MOD

partial def parse (arr : Array String) : Array Nat := Id.run do
  let mut idx := 0
  let mut res : Array Nat := #[]
  for _ in [0:10] do
    let a := arr[idx]!.toNat!; idx := idx + 1
    let b := arr[idx]!.toNat!; idx := idx + 1
    let c := arr[idx]!.toNat!; idx := idx + 1
    let d := arr[idx]!.toNat!; idx := idx + 1
    res := res.push (solveCase a b c d)
  return res

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let outs := parse toks
  for n in outs do
    IO.println n

end SPOJ1459

def main : IO Unit := SPOJ1459.main
