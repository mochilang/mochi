/- Solution for SPOJ SBANK - Sorting Bank Accounts
https://www.spoj.com/problems/SBANK/
-/

import Std
open Std

partial def process (h : IO.FS.Stream) : Nat -> IO Unit
| 0 => pure ()
| Nat.succ t => do
    let n := (← h.getLine).trim.toNat!
    let mut arr : Array String := Array.mkEmpty n
    for _ in [:n] do
      let line := (← h.getLine).trim
      arr := arr.push line
    arr := arr.qsort (fun a b => a ≤ b)
    if arr.size > 0 then
      let mut curr := arr[0]!
      let mut cnt : Nat := 1
      for i in [1:arr.size] do
        let s := arr[i]!
        if s == curr then
          cnt := cnt + 1
        else
          IO.println (curr ++ " " ++ toString cnt)
          curr := s
          cnt := 1
      IO.println (curr ++ " " ++ toString cnt)
    if t > 0 then
      IO.println ""
    try
      let _ ← h.getLine
    catch _ => pure ()
    process h t

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
