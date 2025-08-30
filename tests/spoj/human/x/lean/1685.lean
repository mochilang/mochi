/- Solution for SPOJ GROCERY - Grocery store
https://www.spoj.com/problems/GROCERY/
-/

import Std
open Std

/-- Format a price given in cents as string with two decimals. -/
def formatPrice (x : Nat) : String :=
  let euros := x / 100
  let cents := x % 100
  let centsStr := if cents < 10 then "0" ++ toString cents else toString cents
  s!"{euros}.{centsStr}"

/-- Enumerate all quadruples of prices satisfying sum = product and total ≤ €20. -/
def main : IO Unit := do
  let mut a : Nat := 1
  while a ≤ 500 do
    let mut b : Nat := a
    let maxB := (2000 - a) / 3
    while b ≤ maxB do
      let mut c : Nat := b
      let maxC := (2000 - a - b) / 2
      while c ≤ maxC do
        let prod := a * b * c
        if prod > 1000000 then
          let denom := prod - 1000000
          let num := (a + b + c) * 1000000
          if num % denom == 0 then
            let d := num / denom
            let sum := a + b + c + d
            if d ≥ c && sum ≤ 2000 then
              if prod * d == sum * 1000000 then
                IO.println $
                  formatPrice a ++ " " ++ formatPrice b ++ " " ++
                  formatPrice c ++ " " ++ formatPrice d
        c := c + 1
      b := b + 1
    a := a + 1
