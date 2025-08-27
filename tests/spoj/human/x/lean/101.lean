/- Solution for SPOJ FAMILY - Family
https://www.spoj.com/problems/FAMILY/
-/

import Std
open Std

/-- format a rational probability as percentage string without trailing zeros -/
def formatPercent (r : Rat) : String :=
  let num : Nat := r.num.natAbs * 100
  let den : Nat := r.den
  let intPart := num / den
  let mut rem := num % den
  let mut digits := ""
  while rem ≠ 0 do
    rem := rem * 10
    let d := rem / den
    digits := digits.push (Char.ofNat (48 + d))
    rem := rem % den
  if digits.isEmpty then s!"{intPart}%"
  else s!"{intPart}.{digits}%"

/-- parse all integers from input --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- compute relationship percentages --/
def main : IO Unit := do
  let data ← readInts
  let mut idx := 0
  let t := data.get! idx; idx := idx + 1
  for _ in [0:t] do
    let n := data.get! idx; idx := idx + 1
    let k := data.get! idx; idx := idx + 1
    let mut parents : Array (Option (Nat × Nat)) := Array.mkArray (n+1) none
    let mut children : Array (Array Nat) := Array.mkArray (n+1) #[]
    let mut indeg : Array Nat := Array.mkArray (n+1) 0
    for _ in [0:k] do
      let a := data.get! idx; idx := idx + 1
      let b := data.get! idx; idx := idx + 1
      let c := data.get! idx; idx := idx + 1
      parents := parents.set! a (some (b, c))
      children := children.set! b ((children.get! b).push a)
      children := children.set! c ((children.get! c).push a)
      indeg := indeg.set! a 2
    -- probability vectors
    let mut prob : Array (Array Rat) := Array.mkArray (n+1) (Array.mkArray (n+1) (0 : Rat))
    -- queue for topological order
    let mut q : Array Nat := #[]
    let mut head : Nat := 0
    for i in [1:n+1] do
      if indeg.get! i == 0 then
        prob := prob.set! i ((prob.get! i).set! i (1 : Rat))
        q := q.push i
    while head < q.size do
      let v := q.get! head; head := head + 1
      for c in children.get! v do
        let d := indeg.get! c - 1
        indeg := indeg.set! c d
        if d == 0 then
          match parents.get! c with
          | some (p1, p2) =>
            let pv1 := prob.get! p1
            let pv2 := prob.get! p2
            let mut arr := Array.mkArray (n+1) (0 : Rat)
            for j in [1:n+1] do
              arr := arr.set! j ((pv1.get! j + pv2.get! j) / 2)
            prob := prob.set! c arr
            q := q.push c
          | none => pure ()
    let m := data.get! idx; idx := idx + 1
    for _ in [0:m] do
      let x := data.get! idx; idx := idx + 1
      let y := data.get! idx; idx := idx + 1
      let px := prob.get! x
      let py := prob.get! y
      let mut r : Rat := 0
      for j in [1:n+1] do
        r := r + px.get! j * py.get! j
      IO.println (formatPercent r)
