/- Solution for SPOJ CUBERT - Cube Root
https://www.spoj.com/problems/CUBERT/
-/

import Std
open Std

partial def icbrt (n : Nat) : Nat :=
  let rec loop (lo hi : Nat) : Nat :=
    if hi <= lo + 1 then
      lo
    else
      let mid := (lo + hi) / 2
      let mid3 := mid * mid * mid
      if mid3 <= n then loop mid hi else loop lo mid
  loop 0 (n + 1)

def pow10 (n : Nat) : Nat := Nat.pow 10 n

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (toks[idx]!).toNat!
    let scaled := n * pow10 30
    let r := icbrt scaled
    let intPart := r / pow10 10
    let fracPart := r % pow10 10
    let fracStr :=
      let s := toString fracPart
      let pad := String.mk (List.replicate (10 - s.length) '0')
      pad ++ s
    let digitsStr := toString intPart ++ fracStr
    let checksum :=
      digitsStr.data.foldl (fun acc c => acc + (c.toNat - '0'.toNat)) 0 % 10
    IO.println s!"{checksum} {intPart}.{fracStr}"
    solve toks (idx + 1) (t - 1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "") |>.toArray
  let t := (toks[0]!).toNat!
  solve toks 1 t
