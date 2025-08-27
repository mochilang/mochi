/- Solution for SPOJ EXCHNG - Exchanges
https://www.spoj.com/problems/EXCHNG/
-/

import Std
open Std

partial def solve (tokens : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let m := (tokens.get! (idx+1)).toNat!
    let mut adj : Array (Std.HashSet Nat) := Array.mkArray (n+1) {}
    let mut j := 0
    let mut pos := idx+2
    while j < m do
      let a := (tokens.get! pos).toNat!
      let b := (tokens.get! (pos+1)).toNat!
      let sa := adj.get! a
      let sb := adj.get! b
      adj := adj.set! a (sa.insert b)
      adj := adj.set! b (sb.insert a)
      j := j + 1
      pos := pos + 2
    let mut cnt := 0
    for i in [1:n+1] do
      let deg := (adj.get! i).size
      if deg < 2 then
        cnt := cnt + 1
    IO.println cnt
    solve tokens pos (t-1)

def main : IO Unit := do
  let data <- IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
