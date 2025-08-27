/- Solution for SPOJ COINS - Bytelandian gold coins
https://www.spoj.com/problems/COINS/
-/

import Std
open Std

partial def maxDollar (n : Nat) (memo : Std.HashMap Nat Nat) :
    (Nat × Std.HashMap Nat Nat) :=
  match memo.find? n with
  | some v => (v, memo)
  | none =>
      if n < 12 then
        (n, memo.insert n n)
      else
        let (a, memo) := maxDollar (n / 2) memo
        let (b, memo) := maxDollar (n / 3) memo
        let (c, memo) := maxDollar (n / 4) memo
        let val := Nat.max n (a + b + c)
        (val, memo.insert n val)

partial def process (h : IO.FS.Stream)
    (memo : Std.HashMap Nat Nat) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let line := (← h.getLine).trim
    if line.isEmpty then
      process h memo
    else
      let n := line.toNat!
      let (ans, memo) := maxDollar n memo
      IO.println ans
      process h memo

def main : IO Unit := do
  process (← IO.getStdin) ({} : Std.HashMap Nat Nat)
