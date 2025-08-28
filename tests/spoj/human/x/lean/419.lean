/- Solution for SPOJ TRANSP - Transposing is Fun
https://www.spoj.com/problems/TRANSP/
-/

import Std
open Std

def MOD : Nat := 1000003

partial def powMod (base : Nat) (exp : Nat) : Nat :=
  let rec loop (b e acc : Nat) : Nat :=
    if e == 0 then acc else
      let acc := if e % 2 == 1 then (acc * b) % MOD else acc
      let b := (b * b) % MOD
      loop b (e / 2) acc
  loop (base % MOD) exp 1

partial def phi (n : Nat) : Nat :=
  Id.run do
    let mut res := n
    let mut m := n
    let mut p := 2
    while p * p ≤ m do
      if m % p == 0 then
        while m % p == 0 do
          m := m / p
        res := res / p * (p - 1)
      p := p + 1
    if m > 1 then
      res := res / m * (m - 1)
    return res

partial def divisors (n : Nat) : Array Nat :=
  Id.run do
    let mut arr : Array Nat := #[]
    let mut d := 1
    while d * d ≤ n do
      if n % d == 0 then
        arr := arr.push d
        if d * d ≠ n then
          arr := arr.push (n / d)
      d := d + 1
    return arr

partial def solveCase (a b : Nat) : Nat :=
  if a == 0 && b == 0 then 0 else Id.run do
    let g := Nat.gcd a b
    let a0 := a / g
    let b0 := b / g
    let m0 := a0 + b0
    let divs := divisors m0
    let mut sum := 0
    for d in divs do
      let cnt := phi (m0 / d) % MOD
      let term := cnt * powMod 2 (g * d) % MOD
      sum := (sum + term) % MOD
    let inv := powMod m0 (MOD - 2)
    let cycles := sum * inv % MOD
    let n := powMod 2 (a + b)
    return (n + MOD - cycles) % MOD

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  if toks.size = 0 then return
  let t := toks[0]!.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let a := toks[idx]!.toNat!
    let b := toks[idx+1]!.toNat!
    idx := idx + 2
    IO.println (solveCase a b)
