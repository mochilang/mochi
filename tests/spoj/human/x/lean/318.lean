/- Solution for SPOJ PITPAIR - Pythagorean Legacy
https://www.spoj.com/problems/PITPAIR/
-/

import Std
open Std

/-- simple primality check -/
def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else
    let rec loop (d : Nat) : Bool :=
      if d * d > n then true
      else if n % d == 0 then false
      else loop (d+1)
    loop 2

/-- generate first `k` primes congruent to 1 mod 4 -/
partial def primes1mod4 (k : Nat) : List Nat :=
  let rec aux (p : Nat) (acc : List Nat) : List Nat :=
    if acc.length == k then acc.reverse
    else if p % 4 == 1 && isPrime p then
      aux (p+1) (p :: acc)
    else aux (p+1) acc
  aux 5 []

/-- trial division factorization - returns list of (prime, exponent) -/
partial def factor (n : Nat) : List (Nat × Nat) :=
  let rec aux (m p : Nat) (acc : List (Nat × Nat)) : List (Nat × Nat) :=
    if p * p > m then
      if m > 1 then (m,1)::acc else acc
    else if m % p == 0 then
      let rec count (m c : Nat) :=
        if m % p == 0 then count (m / p) (c+1) else (m,c)
      let (m',c) := count m 0
      aux m' (p+1) ((p,c)::acc)
    else aux m (p+1) acc
  aux n 2 []

/-- exponentiation for `Nat` -/
def pow (a b : Nat) : Nat :=
  let rec aux (a n acc : Nat) : Nat :=
    match n with
    | 0 => acc
    | n+1 => aux a n (acc * a)
  aux a b 1

/-- compute minimal hypotenuse for given N -/
def minimalHyp (N : Nat) : Nat :=
  let M := 2*N + 1
  let fs := factor M
  let exps := fs.foldl (init := []) fun acc (p,e) => acc ++ List.replicate e ((p - 1) / 2)
  let exps := exps.qsort (fun a b => a > b)
  let primes := primes1mod4 exps.length
  (List.zip primes exps).foldl (init := 1) fun acc (p,e) => acc * pow p e

/-- collect shorter catheti for given hypotenuse -/
partial def catheti (c : Nat) : Array Nat :=
  Id.run do
    let mut arr : Array Nat := #[]
    let limit := Nat.sqrt c + 1
    for m in [2:limit+1] do
      for n in [1:m] do
        if (m - n) % 2 == 1 && Nat.gcd m n == 1 then
          let c0 := m*m + n*n
          if c % c0 == 0 then
            let k := c / c0
            let a0 := m*m - n*n
            let b0 := 2*m*n
            let a := k * (if a0 < b0 then a0 else b0)
            arr := arr.push a
    let arr := arr.qsort (· < ·)
    let mut uniq : Array Nat := #[]
    for x in arr do
      if uniq.isEmpty || uniq.back? != some x then
        uniq := uniq.push x
    uniq

partial def solve (N : Nat) : IO Unit := do
  let h := minimalHyp N
  IO.println h
  let cats := catheti h
  for a in cats do
    IO.println a

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let t := line.trim.toNat!
  for _ in [0:t] do
    let nLine ← h.getLine
    let n := nLine.trim.toNat!
    solve n

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
