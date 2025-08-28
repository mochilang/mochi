/- Solution for SPOJ BIRTHDAY - Birthday
https://www.spoj.com/problems/BIRTHDAY/
-/

import Std
open Std

/-- compute minimal total movement to achieve permutation `p` around circular table -/
def solve (n : Nat) (p : Array Nat) : Int :=
  Id.run do
    -- positions of children in permutation
    let mut pos := Array.mkArray (n + 1) 0
    for i in [0:n] do
      pos := pos.set! (p.get! i) i
    let half := n / 2
    let rest := n % 2
    let mut diff := Array.mkArray n (0 : Int)
    let update := fun (L R : Nat) (v : Int) =>
      let Lm := L % n
      let Rm := R % n
      if Lm < Rm then
        diff := diff.set! Lm ((diff.get! Lm) + v)
        diff := diff.set! Rm ((diff.get! Rm) - v)
      else
        diff := diff.set! Lm ((diff.get! Lm) + v)
        diff := diff.set! 0 ((diff.get! 0) + v)
        diff := diff.set! Rm ((diff.get! Rm) - v)
    let mut cost : Int := 0
    for child in [1:n+1] do
      let posc := pos.get! child
      let shift := (child - 1 + n - posc) % n
      let d := if shift < n - shift then shift else n - shift
      cost := cost + Int.ofNat d
      update shift (shift + half) 2
      if rest == 1 then
        update (shift + half) (shift + half + 1) 1
    let mut best := cost
    let mut cur := cost
    let mut acc : Int := 0
    for i in [0:n-1] do
      acc := acc + diff.get! i
      let der := acc - Int.ofNat n
      cur := cur + der
      if cur < best then
        best := cur
    return best

partial def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  if toks.isEmpty then
    return ()
  let n := (toks.get! 0).toNat!
  let mut arr := Array.mkArray n 0
  for i in [0:n] do
    arr := arr.set! i ((toks.get! (i + 1)).toNat!)
  let ans := solve n arr
  IO.println ans
