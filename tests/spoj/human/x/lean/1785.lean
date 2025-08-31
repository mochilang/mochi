/- Solution for SPOJ CODE - Code
https://www.spoj.com/problems/CODE/
-/

import Std
open Std

/-- Generate the de Bruijn sequence for alphabet of size `k` and subsequences of length `n`.
    Returns a list of digits. --/
partial def deBruijn (k n : Nat) : List Nat :=
  let rec db (t p : Nat) (a : Array Nat) (seq : List Nat) : (Array Nat × List Nat) :=
    if t > n then
      if n % p == 0 then
        let rec add (i : Nat) (seq : List Nat) : List Nat :=
          if i ≤ p then
            add (i + 1) (a[i]! :: seq)
          else seq
        (a, add 1 seq)
      else
        (a, seq)
    else
      let a := a.set! t (a[t - p]!)
      let (a, seq) := db (t + 1) p a seq
      let rec loop (j : Nat) (a : Array Nat) (seq : List Nat) : (Array Nat × List Nat) :=
        if j < k then
          let a := a.set! t j
          let (a, seq) := db (t + 1) t a seq
          loop (j + 1) a seq
        else
          (a, seq)
      loop ((a[t - p]!) + 1) a seq
  let (_, seq) := db 1 1 (Array.replicate (k * n + 1) 0) []
  seq.reverse

/-- Build de Bruijn string for base 10 and given length `n`. --/
def build (n : Nat) : String :=
  let seq := deBruijn 10 n
  let pref := seq.take (n - 1)
  let all := seq ++ pref
  let chars := all.map (fun d => Char.ofNat (d + 48))
  String.mk chars

partial def loop (arr : Array Nat) (i : Nat) : IO Unit := do
  if i < arr.size then
    let n := arr[i]!
    if n == 0 then
      pure ()
    else
      IO.println (build n)
      loop arr (i + 1)
  else
    pure ()

/-- Read all natural numbers from stdin. --/
def readNats : IO (Array Nat) := do
  let stdin ← IO.getStdin
  let s ← stdin.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Main program --/
def main : IO Unit := do
  let nums ← readNats
  loop nums 0
