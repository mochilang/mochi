/- Solution for SPOJ MONODIG - Monodigital Representations
https://www.spoj.com/problems/MONODIG/
-/

import Std
open Std

/-- Repeat digit `k` exactly `n` times to form a number like `kkk`. -/
def repeatDigit (k n : Nat) : Nat :=
  (List.range n).foldl (fun acc _ => acc * 10 + k) 0

/-- Precompute minimal lengths of `k`-representations for numbers `<= maxVal`. -/
def precompute (k : Nat) (maxVal : Nat := 32000) : Array (Option Nat) :=
  let maxLen := 8
  Id.run do
    let mut dp : Array (Option Nat) := Array.mkArray (maxVal+1) none
    let mut sets : Array (Array Nat) := Array.mkArray (maxLen+1) #[]
    -- numbers composed solely of digit k
    for l in [1:maxLen+1] do
      let num := repeatDigit k l
      if num ≤ maxVal then
        dp := dp.set! num (some l)
        sets := sets.set! l (#[num])
      else
        sets := sets.set! l #[]
    -- combine shorter representations
    for l in [1:maxLen+1] do
      for i in [1:l] do
        let j := l - i
        if j > 0 then
          for x in sets.get! i do
            for y in sets.get! j do
              let update := fun (v : Nat) =>
                if v > 0 && v ≤ maxVal && dp.get! v == none then
                  dp := dp.set! v (some l)
                  sets := sets.set! l ((sets.get! l).push v)
              update (x + y)
              if x > y then update (x - y)
              if y > x then update (y - x)
              let prod := x * y
              if prod ≤ maxVal then update prod
              if y != 0 && x % y == 0 then update (x / y)
              if x != 0 && y % x == 0 then update (y / x)
    return dp

/-- Main solving procedure. -/
partial def mainLoop (toks : Array String) (idx : Nat) : IO Unit := do
  if idx < toks.size then
    let k := (toks.get! idx).toNat!
    let n := (toks.get! (idx+1)).toNat!
    let dp := precompute k
    let mut j := idx + 2
    for _ in [0:n] do
      let a := (toks.get! j).toNat!
      j := j + 1
      match dp.get! a with
      | some len => IO.println (toString len)
      | none => IO.println "NO"
    mainLoop toks j
  else
    pure ()

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
              |>.filter (· ≠ "") |> Array.ofList
  mainLoop toks 1
