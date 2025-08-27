/- Solution for SPOJ ROCK - Sweet and Sour Rock
https://www.spoj.com/problems/ROCK
-/

import Std
open Std

private def solveCase (s : String) : Nat :=
  let n := s.length
  let arr := s.data.toArray
  let pref := Id.run do
    let mut p := Array.replicate (n + 1) 0
    for i in [0:n] do
      let bit := if arr[i]! = '1' then 1 else 0
      p := p.set! (i+1) (p[i]! + bit)
    return (p : Array Nat)
  let dp := Id.run do
    let mut d := Array.replicate (n + 1) 0
    for r in [1:n+1] do
      let mut best := d[r - 1]!
      for l in [0:r] do
        let ones := pref[r]! - pref[l]!
        let len := r - l
        if ones * 2 > len then
          let cand := d[l]! + len
          if cand > best then
            best := cand
      d := d.set! r best
    return (d : Array Nat)
  dp[n]!

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let _n := (← h.getLine).trim.toNat!
    let s := (← h.getLine).trim
    IO.println (solveCase s)
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
