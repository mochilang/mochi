/- Solution for SPOJ MOBILE2 - Mobiles
https://www.spoj.com/problems/MOBILE2/
-/

import Std
open Std

structure Info where
  cost : Nat
  minDepth : Nat
  maxDepth : Nat
  deriving Inhabited

def combine (a b : Info) (swap : Bool) : Option Info :=
  let mn := min a.minDepth b.minDepth
  let mx := max a.maxDepth b.maxDepth
  if mx ≤ mn + 1 ∧ b.maxDepth ≤ a.minDepth then
    some { cost := a.cost + b.cost + (if swap then 1 else 0)
          , minDepth := mn, maxDepth := mx }
  else
    none

def solve (n : Nat) (ls rs : Array Int) : Option Info :=
  Id.run do
    let mut dp : Array (Option Info) := Array.mkArray (n+1) none
    for idx in [0:n] do
      let i := n - idx
      let l := ls[i]!
      let r := rs[i]!
      let li? :=
        if l == -1 then
          some { cost := 0, minDepth := 1, maxDepth := 1 }
        else
          match dp[l.toNat]! with
          | some info => some { cost := info.cost, minDepth := info.minDepth + 1, maxDepth := info.maxDepth + 1 }
          | none => none
      let ri? :=
        if r == -1 then
          some { cost := 0, minDepth := 1, maxDepth := 1 }
        else
          match dp[r.toNat]! with
          | some info => some { cost := info.cost, minDepth := info.minDepth + 1, maxDepth := info.maxDepth + 1 }
          | none => none
      let res? :=
        match li?, ri? with
        | some li, some ri =>
            let o1 := combine li ri false
            let o2 := combine ri li true
            match o1, o2 with
            | some a, some b => if a.cost ≤ b.cost then some a else some b
            | some a, none => some a
            | none, some b => some b
            | none, none => none
        | _, _ => none
      dp := dp.set! i res?
    return dp[1]!

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let n := (← h.getLine).trim.toNat!
    let mut ls : Array Int := Array.mkArray (n+1) 0
    let mut rs : Array Int := Array.mkArray (n+1) 0
    for i in [1:n+1] do
      let line ← h.getLine
      let parts := line.trim.split (· = ' ')
      let l := parts.get! 0 |>.toInt!
      let r := parts.get! 1 |>.toInt!
      ls := ls.set! i l
      rs := rs.set! i r
    match solve n ls rs with
    | some info => IO.println info.cost
    | none => IO.println (-1)
