/- Solution for SPOJ WSCIPHER - Ws Cipher
https://www.spoj.com/problems/WSCIPHER/
-/

import Std
open Std

-- rotate characters at given positions to the right by k steps
private def rotateGroup (arr : Array Char) (pos : Array Nat) (k : Nat) : Array Char :=
  let n := pos.size
  if n = 0 then
    arr
  else
    let k := k % n
    let mut vals : Array Char := #[]
    for j in [0:n] do
      vals := vals.push (arr.get! (pos.get! j))
    let mut res := arr
    for j in [0:n] do
      let idx := pos.get! j
      let ch := vals.get! ((j + n - k) % n)
      res := res.set! idx ch
    res

-- decrypt message using rotations
private def decrypt (k1 k2 k3 : Nat) (s : String) : String :=
  let arr := s.data.toArray
  let mut g1 : Array Nat := #[]
  let mut g2 : Array Nat := #[]
  let mut g3 : Array Nat := #[]
  for i in [0:arr.size] do
    let c := arr.get! i
    if decide ('a' ≤ c ∧ c ≤ 'i') then
      g1 := g1.push i
    else if decide ('j' ≤ c ∧ c ≤ 'r') then
      g2 := g2.push i
    else
      g3 := g3.push i
  let arr := rotateGroup arr g1 k1
  let arr := rotateGroup arr g2 k2
  let arr := rotateGroup arr g3 k3
  String.mk arr.toList

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let nums := line.split (· = ' ')
                |>.filter (· ≠ "")
                |>.map (·.toNat!)
  match nums with
  | [k1, k2, k3] =>
      if k1 = 0 ∧ k2 = 0 ∧ k3 = 0 then
        pure ()
      else
        let msg := (← h.getLine).trim
        IO.println (decrypt k1 k2 k3 msg)
        loop h
  | _ => pure ()

def main : IO Unit :=
  loop (← IO.getStdin)
