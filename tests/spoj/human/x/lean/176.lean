/- Solution for SPOJ SUM1SEQ - Sum of one-sequence
https://www.spoj.com/problems/SUM1SEQ/
-/
import Std
open Std

def oneSeq (n : Nat) (S : Int) : Option (Array Int) :=
  let nInt : Int := Int.ofNat n
  let total : Int := nInt * (nInt - 1) / 2
  let absS := if S < 0 then -S else S
  if absS > total || ((S - total) % 2 != 0) then
    none
  else
    let mut target := S
    let mut w : Int := nInt - 1
    let mut steps : Array Int := Array.mkEmpty (n - 1)
    while w > 0 do
      let sign : Int := if target >= 0 then 1 else -1
      steps := steps.push sign
      target := target - sign * w
      w := w - 1
    let mut seq : Array Int := Array.mkEmpty n
    let mut cur : Int := 0
    seq := seq.push cur
    for s in steps do
      cur := cur + s
      seq := seq.push cur
    some seq

partial def solve (tokens : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let s := (tokens.get! (idx+1)).toInt!
    match oneSeq n s with
    | none => IO.println "No"
    | some arr =>
        for x in arr do
          IO.println x
    if t > 1 then
      IO.println ""
    solve tokens (idx+2) (t-1)

def main : IO Unit := do
  let data <- IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
