/- Solution for SPOJ ROBOT - Robot Number M
https://www.spoj.com/problems/ROBOT/
-/

import Std
open Std

partial def powMod (b e m : Nat) : Nat :=
  if e == 0 then
    1 % m
  else
    let h := powMod b (e / 2) m
    let h := (h * h) % m
    if e % 2 == 1 then
      (h * (b % m)) % m
    else
      h

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let k := line.toNat!
      let mut mmod : Nat := 1
      let mut prod : Nat := 1
      let mut alt : Nat := 1
      for _ in [0:k] do
        let ln := (← h.getLine).trim
        if ln = "" then
          pure ()
        else
          let ws := ln.split (· = ' ') |>.filter (· ≠ "")
          let p := ws[0]! |>.toNat!
          let a := ws[1]! |>.toNat!
          mmod := (mmod * powMod p a 10000) % 10000
          if p % 2 == 1 then
            prod := (prod * (p % 20000)) % 20000
            let term := (20002 - (p % 20000)) % 20000
            alt := (alt * term) % 20000
      let even := ((prod + alt) % 20000) / 2
      let odd := ((prod + 20000 - alt) % 20000) / 2
      let sumB := (even + 10000 - 1) % 10000
      let sumH := odd % 10000
      let total := (mmod + 10000 - 1) % 10000
      let sumD := (total + 20000 - sumB - sumH) % 10000
      IO.println sumB
      IO.println sumH
      IO.println sumD
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
