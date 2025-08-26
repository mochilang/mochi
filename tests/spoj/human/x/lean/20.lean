/- Solution for SPOJ CRYPTO4 - The Bytelandian Cryptographer (Act IV)
https://www.spoj.com/problems/CRYPTO4/
-/

import Std
open Std

def c2i (c : Char) : Int := (c.toNat - 'A'.toNat)

def i2c (i : Int) : Char :=
  let v := Int.toNat ((i % 26 + 26) % 26)
  Char.ofNat (v + 'A'.toNat)

partial def solveCase (m : Nat) (x y : String) : String :=
  let L := x.length
  let xs := x.data
  let ys := y.data
  let mut res : Array (Option Char) := Array.mkArray L none
  let mut first := true
  for n in [1:m+1] do
    let mut key : Array (Option Int) := Array.mkArray n none
    let mut ok := true
    for i in [0:L] do
      let cx := xs.get! i
      let cy := ys.get! i
      if cx != '*' && cy != '*' then
        let r := ((c2i cy) - (c2i cx) + 26) % 26
        let j := i % n
        match key.get! j with
        | some v => if v != r then ok := false
        | none   => key := key.set! j (some r)
    if ok then
      let mut cand : Array (Option Char) := Array.mkArray L none
      for i in [0:L] do
        let cx := xs.get! i
        if cx != '*' then
          cand := cand.set! i (some cx)
        else
          let cy := ys.get! i
          if cy != '*' then
            match key.get! (i % n) with
            | some r =>
                let ch := i2c ((c2i cy) - r)
                cand := cand.set! i (some ch)
            | none => pure ()
      if first then
        res := cand
        first := false
      else
        for i in [0:L] do
          match res.get! i with
          | some o =>
              match cand.get! i with
              | some c => if c != o then res := res.set! i none
              | none   => res := res.set! i none
          | none => pure ()
  let chars := (List.range L).map (fun i => match res.get! i with | some c => c | none => '*')
  String.mk chars

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let m := (← h.getLine).trim.toNat!
    let x := (← h.getLine).trim
    let y := (← h.getLine).trim
    IO.println (solveCase m x y)
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
