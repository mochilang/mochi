/- Solution for SPOJ PON - Is it prime?
https://www.spoj.com/problems/PON/
-/

import Std
open Std

/-- modular exponentiation by repeated squaring --/
partial def powMod (a b m : Nat) : Nat :=
  if b == 0 then 1 % m
  else
    let h := powMod a (b / 2) m
    let hh := (h * h) % m
    if b % 2 == 0 then hh else (hh * (a % m)) % m

/-- write n-1 as d * 2^s with d odd --/
partial def decompose (d s : Nat) : Nat × Nat :=
  if d % 2 == 0 then decompose (d / 2) (s + 1) else (d, s)

/-- check whether a is a Miller-Rabin witness for composite n --/
def witness (a n : Nat) : Bool :=
  let (d, s) := decompose (n - 1) 0
  let x := powMod a d n
  if x == 1 || x == n - 1 then
    false
  else
    let rec loop (r x : Nat) : Bool :=
      if r == s then true
      else
        let x := (x * x) % n
        if x == n - 1 then false else loop (r + 1) x
    loop 1 x

/-- deterministic bases for 64-bit Miller-Rabin --/
def bases : List Nat := [2, 3, 5, 7, 11, 13]

/-- primality test for numbers < 2^63 using Miller-Rabin --/
def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else if n % 2 == 0 then n == 2
  else
    let rec check : List Nat → Bool
    | [] => true
    | a :: rest =>
        if a ≥ n then true
        else if n % a == 0 then false
        else if witness a n then false
        else check rest
    check bases

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (if isPrime n then "YES" else "NO")
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  process h (tLine.trim.toNat!)
