/- Solution for SPOJ TRANSP2 - Transposing is Even More Fun
https://www.spoj.com/problems/TRANSP2/
-/

import Std
open Std

-- fast exponentiation modulo m
partial def powMod (b : Nat) (e : Nat) (m : Nat) : Nat :=
  let rec loop (b e acc : Nat) :=
    if e = 0 then acc
    else
      let acc := if e % 2 = 1 then (acc * b) % m else acc
      loop (b * b % m) (e / 2) acc
  loop (b % m) e 1

-- modular inverse using Fermat's little theorem (m is prime)
def invMod (a m : Nat) : Nat :=
  powMod a (m - 2) m

-- trial division factorization
partial def factors (n : Nat) (p : Nat := 2) (acc : List (Nat × Nat) := []) : List (Nat × Nat) :=
  if n = 1 then acc.reverse
  else if p * p > n then (acc.reverse) ++ [(n,1)]
  else if n % p = 0 then
    let rec count (n c : Nat) :=
      if n % p = 0 then count (n / p) (c + 1) else (n, c)
    let (n', c) := count n 0
    factors n' (p+1) ((p,c)::acc)
  else
    factors n (p+1) acc

-- produce list of (p^i, phi(p^i)) for i = 0..e
partial def powPhi (p e : Nat) : List (Nat × Nat) :=
  let rec loop (i pow : Nat) (acc : List (Nat × Nat)) :=
    if i > e then acc.reverse
    else
      let phi := if i = 0 then 1 else (pow / p) * (p - 1)
      loop (i + 1) (pow * p) ((pow, phi) :: acc)
  loop 0 1 []

-- generate all divisors q of h along with phi(q)
partial def divisorsWithPhi (fs : List (Nat × Nat)) : List (Nat × Nat) :=
  match fs with
  | [] => [(1,1)]
  | (p,e)::rest =>
      let tail := divisorsWithPhi rest
      tail.bind (fun (d,phi_d) =>
        (powPhi p e).map (fun (pw,phi_p) => (d * pw, phi_d * phi_p))
      )

-- solve single test case
partial def solveCase (a b : Nat) : Nat :=
  let MOD := 1000003
  if a = 0 ∧ b = 0 then
    0
  else
    let L := a + b
    let g := Nat.gcd a b
    let h := L / g
    let fs := factors h
    let divs := divisorsWithPhi fs
    let sum := divs.foldl (fun acc (q,phi_q) =>
        let d := h / q
        let term := (phi_q % MOD) * powMod 2 (g * d) MOD % MOD
        (acc + term) % MOD
      ) 0
    let cycles := sum * invMod h MOD % MOD
    let total := powMod 2 L MOD
    (total + MOD - cycles) % MOD

-- parse input tokens to Nats
def parseNums (tokens : List String) : List Nat :=
  tokens.map String.toNat!

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let tokens := input.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let nums := parseNums tokens
  if nums.isEmpty then
    pure ()
  else
    let t := nums.head!
    let rec loop (i idx : Nat) : IO Unit := do
      if i = t then
        pure ()
      else
        let a := nums.get! idx
        let b := nums.get! (idx + 1)
        IO.println (solveCase a b)
        loop (i + 1) (idx + 2)
    loop 0 1
