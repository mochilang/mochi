/- Solution for SPOJ PT07D - Let us count 1 2 3
https://www.spoj.com/problems/PT07D/
-/
import Std
open Std

-- fast modular exponentiation
partial def powMod (a b m : Nat) : Nat :=
  let rec go (a b res : Nat) : Nat :=
    if b == 0 then res
    else
      let res := if b % 2 == 1 then (res * a) % m else res
      go ((a * a) % m) (b / 2) res
  go (a % m) b 1

-- modular inverse modulo prime p
partial def modInv (a p : Nat) : Nat :=
  powMod a (p - 2) p

-- compute array of rooted unlabeled trees counts modulo p up to n
partial def rootedArr (n p : Nat) : Array Nat :=
  Id.run do
    let mut a : Array Nat := Array.mkArray (n + 1) 0
    let mut b : Array Nat := Array.mkArray (n + 1) 0
    if n ≥ 1 then
      a := a.set! 1 (1 % p)
      for j in [1:n+1] do
        b := b.set! j ((b.get! j + 1) % p)
      for m in [2:n+1] do
        let mut s := 0
        for k in [1:m] do
          s := (s + b.get! k * a.get! (m - k)) % p
        let inv := modInv (m - 1) p
        let am := (s * inv) % p
        a := a.set! m am
        let mut j := m
        while j ≤ n do
          b := b.set! j ((b.get! j + m * am) % p)
          j := j + m
    return a

partial def countRooted (n p : Nat) : Nat :=
  (rootedArr n p).get! n

partial def countUnrooted (n p : Nat) : Nat :=
  if p == 2 then
    1 % p
  else
    let arr := rootedArr n p
    let mut s := 0
    for k in [1:n] do
      s := (s + arr.get! k * arr.get! (n - k)) % p
    let inv2 := modInv 2 p
    let sub := (s * inv2) % p
    if n % 2 == 0 then
      let add := (arr.get! (n / 2) * inv2) % p
      (arr.get! n + p - sub + add) % p
    else
      (arr.get! n + p - sub) % p

partial def process (h : IO.FS.Stream) : IO Unit := do
  if (← h.isEof) then
    pure ()
  else
    let line := (← h.getLine).trim
    if line == "" then
      process h
    else
      let parts := line.split (· = ' ') |>.filter (· ≠ "")
      if parts.size < 3 then
        process h
      else
        let k := parts[0]! |>.toNat!
        let n := parts[1]! |>.toNat!
        let p := parts[2]! |>.toNat!
        let ans :=
          match k with
          | 1 => powMod n (n - 2) p
          | 2 => powMod n (n - 1) p
          | 3 => countRooted n p
          | 4 => countUnrooted n p
          | _ => 0
        IO.println ans
        process h

def main : IO Unit := do
  process (← IO.getStdin)
