/- Solution for SPOJ MENU - Menu
https://www.spoj.com/problems/MENU/
-/

import Std
open Std

structure Dish where
  cost : Nat
  val  : Nat

/-- dynamic programming solution returning best value*2 and chosen plan -/
private def calc (k n m : Nat) (dishes : Array Dish) : (Int × List Nat) :=
  Id.run do
    let states := (k+1)*(m+1)*(n+1)*3
    let mut dp : Array Int := Array.mkArray states (-1)
    let mut pc : Array Nat := Array.mkArray states 0
    let mut pl : Array Nat := Array.mkArray states 0
    let mut pr : Array Nat := Array.mkArray states 0
    let idx := fun (day cost last rep : Nat) =>
      (((day * (m+1) + cost) * (n+1) + last) * 3 + rep)
    -- initial state
    dp := dp.set! (idx 0 0 0 0) 0
    for day in [0:k] do
      for cost in [0:m+1] do
        for last in [0:n+1] do
          for rep in [0:3] do
            let id := idx day cost last rep
            let cur := dp[id]!
            if cur >= 0 then
              for j in [0:n] do
                let d := dishes[j]!
                let newCost := cost + d.cost
                if newCost ≤ m then
                  let dishIdx := j + 1
                  let (newRep, gain) :=
                    if dishIdx == last then
                      if rep == 1 then (2, d.val)
                      else (2, 0)
                    else
                      (1, 2 * d.val)
                  let id2 := idx (day + 1) newCost dishIdx newRep
                  let cand := cur + (Int.ofNat gain)
                  if cand > dp[id2]! then
                    dp := dp.set! id2 cand
                    pc := pc.set! id2 cost
                    pl := pl.set! id2 last
                    pr := pr.set! id2 rep
    let mut bestVal : Int := -1
    let mut bestCost : Nat := 0
    let mut bestLast : Nat := 0
    let mut bestRep : Nat := 0
    for cost in [0:m+1] do
      for last in [0:n+1] do
        for rep in [0:3] do
          let id := idx k cost last rep
          let v := dp[id]!
          if v > bestVal || (v == bestVal && cost < bestCost) then
            bestVal := v
            bestCost := cost
            bestLast := last
            bestRep := rep
    if bestVal < 0 then
      return (0, [])
    else
      let mut plan : List Nat := []
      let mut day := k
      let mut cost := bestCost
      let mut last := bestLast
      let mut rep := bestRep
      while day > 0 do
        plan := last :: plan
        let id := idx day cost last rep
        let pc0 := pc[id]!
        let pl0 := pl[id]!
        let pr0 := pr[id]!
        day := day - 1
        cost := pc0
        last := pl0
        rep := pr0
      return (bestVal, plan)

private partial def process (toks : Array String) (i : Nat) (acc : List String) : List String :=
  if h : i + 2 < toks.size then
    let k := toks[i]!.toNat!
    let n := toks[i+1]!.toNat!
    let m := toks[i+2]!.toNat!
    if k == 0 && n == 0 && m == 0 then
      acc.reverse
    else
      let mut dishes : Array Dish := Array.mkEmpty n
      let mut j := i + 3
      for _ in [0:n] do
        let c := toks[j]!.toNat!
        let v := toks[j+1]!.toNat!
        dishes := dishes.push {cost := c, val := v}
        j := j + 2
      let (val, plan) := calc k n m dishes
      let intPart := Int.toNat (val / 2)
      let frac := if val % 2 == 0 then "0" else "5"
      let line1 := s!"{intPart}.{frac}"
      let acc := line1 :: acc
      let acc := if plan.isEmpty then acc
        else (String.intercalate " " (plan.map toString)) :: acc
      process toks j acc
  else
    acc.reverse

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let lines := process toks 0 []
    for line in lines do
      IO.println line
