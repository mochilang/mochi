/- Solution for SPOJ DANCE - The Gordian Dance
https://www.spoj.com/problems/DANCE/
-/

import Std
open Std

-- compute absolute value for Int
def absInt (x : Int) : Int := if x < 0 then -x else x

-- greatest common divisor for Ints
def gcdInt (a b : Int) : Int := Int.ofNat (Nat.gcd a.natAbs b.natAbs)

-- ceiling division a/b for positive b
def ceilDiv (a b : Int) : Int := (a + b - 1) / b

-- recursively compute minimal moves from fraction p/q (p ≥ 0, q ≥ 0)
partial def rec (p q : Int) : Int :=
  if p = 0 then
    0
  else if q = 0 then
    1
  else if p >= q then
    let k := p / q
    let r := p % q
    if r = 0 then
      k * 3 - 1
    else
      k * 3 + rec r q
  else
    let k := q / p
    let r := q % p
    k + rec p r

-- evaluate the sequence of moves and return fraction p/q
partial def evalMoves (s : String) : Int × Int :=
  let arr := s.data.toArray
  let n := arr.size
  let rec loop (i : Nat) (p q : Int) : Int × Int :=
    if i < n then
      let c := arr[i]!
      if c = 'S' then
        if q ≠ 0 then
          loop (i+1) (p + q) q
        else
          loop (i+1) p q
      else -- 'R'
        if p = 0 then
          loop (i+1) 1 0
        else
          loop (i+1) (-q) p
    else
      (p, q)
  loop 0 0 1

partial def process (h : IO.FS.Stream) (cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let nLine ← h.getLine
    let _ := nLine.trim.toNat! -- length is ignored after validation
    let moves ← h.getLine
    let (p0, q0) := evalMoves moves.trim
    let mut p := p0
    let mut q := q0
    if q < 0 then
      p := -p
      q := -q
    let g := gcdInt p q
    if g ≠ 0 then
      p := p / g
      q := q / g
    let mut ans : Int := 0
    if p < 0 then
      let k := ceilDiv (-p) q
      ans := ans + k
      p := p + k * q
    ans := ans + rec p q
    IO.println (toString ans)
    process h (cases - 1)

def main : IO Unit := do
  process (← IO.getStdin) 10
