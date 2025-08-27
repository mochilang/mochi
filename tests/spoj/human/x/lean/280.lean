/- Solution for SPOJ LIFTS - Lifts
https://www.spoj.com/problems/LIFTS/
-/

import Std
open Std

structure Lift where
  l2 : Int
  u2 : Int
  len2 : Int
  offset : Int

def mkLift (l u s d : Int) : Lift :=
  let l2 := 2 * l
  let u2 := 2 * u
  let len2 := u2 - l2
  if len2 = 0 then
    { l2 := l2, u2 := u2, len2 := 0, offset := 0 }
  else
    let s2 := 2 * s
    let phase := s2 - l2
    let off := if d = 1 then phase else (2 * len2 - phase)
    { l2 := l2, u2 := u2, len2 := len2, offset := off }

/-- height at time t (in half seconds) scaled by 2 --/
def height2 (L : Lift) (t : Int) : Int :=
  if L.len2 = 0 then
    L.l2
  else
    let m := 2 * L.len2
    let cycle := Int.emod (L.offset + t) m
    if cycle < L.len2 then
      L.l2 + cycle
    else
      L.u2 - (cycle - L.len2)

/-- period in half-second units --/
def period2 (L : Lift) : Int :=
  if L.len2 = 0 then 1 else 2 * L.len2

/-- lcm for Ints --/
def lcmInt (a b : Int) : Int :=
  Int.ofNat (Nat.lcm (Int.natAbs a) (Int.natAbs b))

/-- earliest time >= t0 where heights equal, in half seconds --/
def findAlign (a b : Lift) (t0 : Int) : Option Int :=
  let p := lcmInt (period2 a) (period2 b)
  let limit := t0 + p
  Id.run do
    let mut t := t0
    let mut ans : Option Int := none
    while (decide (t ≤ limit)) && ans.isNone do
      if height2 a t = height2 b t then
        ans := some t
      else
        t := t + 1
    return ans

/-- solve one test case --/
def solveCase (lifts : Array Lift) (n : Nat) : Option Int :=
  let mut cur : Int := 0
  let mut ok := true
  for i in [0:n+1] do
    if ok then
      let t0 := cur + 2
      match findAlign lifts[i]! lifts[i+1]! t0 with
      | some s => cur := s + 2
      | none   => ok := false
  if ok then some cur else none

partial def solveAll (toks : Array String) (idx cases : Nat) (acc : List String) : List String :=
  if cases = 0 then acc.reverse else
    let n := toks[idx]!.toNat!
    let default := mkLift 0 0 0 1
    let mut arr : Array Lift := Array.mkArray (n+2) default
    let mut j := idx + 1
    for k in [0:n] do
      let l := toks[j]!.toInt!
      let u := toks[j+1]!.toInt!
      let s := toks[j+2]!.toInt!
      let d := toks[j+3]!.toInt!
      arr := arr.set! (k+1) (mkLift l u s d)
      j := j + 4
    match solveCase arr n with
    | some t =>
        let q := t / 2
        let r := t % 2
        let out := if r = 0 then s!"{q}" else s!"{q}.5"
        solveAll toks j (cases-1) (out :: acc)
    | none =>
        solveAll toks j (cases-1) ("-1" :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
