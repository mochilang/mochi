/- Solution for SPOJ SPIN - Spin
https://www.spoj.com/problems/SPIN/
-/

import Std
open Std

abbrev Key := Nat × Nat × Nat × Nat

partial def dist (memo : IO.Ref (Std.HashMap Key Nat))
    (N S x Q0 : Nat) : IO Nat := do
  if N == 1 then
    if S == Q0 then
      return x
    else
      return 3 - x
  else
    let key : Key := (N, S, x, Q0)
    let mp ← memo.get
    match mp.get? key with
    | some v => return v
    | none => do
        let r ←
          if (S &&& 1) == Q0 then
            let r ← dist memo (N - 1) (S >>> 1) (x - 1) 0
            pure (1 + r)
          else
            let a ← dist memo (N - 1) (S >>> 1) (x - 1) 1
            let b ← dist memo (N - 1) 1 0 0
            pure (2 + a + b)
        memo.modify (fun m => m.insert key r)
        return r

partial def solveCase (memo : IO.Ref (Std.HashMap Key Nat))
    (N : Nat) (s : String) (x : Nat) : IO Nat := do
  let (_, S) := s.data.foldl
    (fun (acc : Nat × Nat) c =>
      let (i, v) := acc
      let v' := if c == 'v' then v ||| (1 <<< i) else v
      (i+1, v'))
    (0, 0)
  let r ← dist memo N S x 0
  return r + 1

partial def loop (memo : IO.Ref (Std.HashMap Key Nat))
    (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let parts := line.trim.splitOn " "
    let N := parts.get! 0 |>.toNat!
    let s := parts.get! 1
    let x := parts.get! 2 |>.toNat!
    let ans ← solveCase memo N s x
    IO.println ans
    loop memo h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let memo ← IO.mkRef (Std.HashMap.empty : Std.HashMap Key Nat)
  loop memo h t
