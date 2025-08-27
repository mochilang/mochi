/- Solution for SPOJ ACT - Alpha Centauri Tennis
https://www.spoj.com/problems/ACT/
-/

import Std
open Std

def winner (n : Nat) (s : String) : Nat :=
  Id.run do
    let mut points := Array.mkArray n 0
    let mut games := Array.mkArray n 0
    let mut sets := Array.mkArray n 0

    let winGame := fun (p : Nat) => do
      games := games.set! p ((games.get! p) + 1)
      points := Array.mkArray n 0
      let g := games.get! p
      let mut maxOther := 0
      for i in [0:n] do
        if i != p then
          let v := games.get! i
          if v > maxOther then
            maxOther := v
      if g >= 6 && g >= maxOther + 2 then
        let add := if maxOther = 0 then 2 else 1
        sets := sets.set! p ((sets.get! p) + add)
        games := Array.mkArray n 0

    for ch in s.data do
      let p := ch.toNat - 'A'.toNat
      let pPts := points.get! p
      if pPts == 3 then
        let mut ok := true
        for i in [0:n] do
          if i != p && points.get! i > 2 then
            ok := false
        if ok then
          winGame p
        else
          points := points.set! p (pPts + 1)
      else if pPts == 4 then
        winGame p
      else
        let mut other4 : Option Nat := none
        for i in [0:n] do
          if i != p && points.get! i == 4 then
            other4 := some i
        match other4 with
        | some q =>
            points := points.set! q 3
            points := points.set! p (pPts + 1)
        | none =>
            points := points.set! p (pPts + 1)
    let mut ans := 0
    for i in [0:n] do
      if sets.get! i >= 3 then
        ans := i
    return ans

partial def solve (tokens : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let s := tokens.get! (idx + 1)
    let w := winner n s
    let ch := Char.ofNat ('A'.toNat + w)
    IO.println (String.singleton ch)
    solve tokens (idx + 2) (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
