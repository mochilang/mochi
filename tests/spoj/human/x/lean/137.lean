/- Solution for SPOJ PARTIT - Partition
https://www.spoj.com/problems/PARTIT/
-/

import Std
open Std

abbrev Key := Nat × Nat × Nat

partial def count (memo : IO.Ref (Std.HashMap Key Nat))
    (m n min : Nat) : IO Nat := do
  if n == 0 then
    return if m == 0 then 1 else 0
  else if m < n * min then
    return 0
  else
    let mp ← memo.get
    match mp.get? (m, n, min) with
    | some v => return v
    | none => do
        let maxVal := m / n
        let mut total : Nat := 0
        for a in [min:maxVal+1] do
          let c ← count memo (m - a) (n - 1) a
          total := total + c
        memo.modify (fun mp => mp.insert (m, n, min) total)
        return total

partial def kth (memo : IO.Ref (Std.HashMap Key Nat))
    (m n min k : Nat) : IO (List Nat) := do
  if n == 0 then
    return []
  else
    let maxVal := m / n
    let rec search (a k : Nat) : IO (List Nat) := do
      if a > maxVal then
        return []
      else
        let c ← count memo (m - a) (n - 1) a
        if k ≤ c then
          let rest ← kth memo (m - a) (n - 1) a k
          return a :: rest
        else
          search (a + 1) (k - c)
    search min k

partial def solveCase (memo : IO.Ref (Std.HashMap Key Nat))
    (m n k : Nat) : IO String := do
  let parts ← kth memo m n 1 k
  return String.intercalate " " (parts.map (fun x => toString x))

partial def loop (h : IO.FS.Stream)
    (memo : IO.Ref (Std.HashMap Key Nat)) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let m := (← h.getLine).trim.toNat!
    let n := (← h.getLine).trim.toNat!
    let k := (← h.getLine).trim.toNat!
    IO.println (← solveCase memo m n k)
    loop h memo (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let memo ← IO.mkRef (Std.HashMap.empty : Std.HashMap Key Nat)
  loop h memo t
