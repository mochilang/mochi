/- Solution for SPOJ QUEST4 - Dungeon of Death
https://www.spoj.com/problems/QUEST4/
-/

import Std
open Std

partial def dfs (r : Nat) (adj : Array (Array Nat))
    (visRef : IO.Ref (Array Bool)) (matchRef : IO.Ref (Array (Option Nat))) : IO Bool := do
  let row := adj[r]!
  let rec loop (i : Nat) : IO Bool := do
    if h : i < row.size then
      let _ := h
      let c := row[i]!
      let vis ← visRef.get
      if vis[c]! then
        loop (i+1)
      else
        visRef.modify (fun arr => arr.set! c true)
        let m ← matchRef.get
        match m[c]! with
        | none =>
            matchRef.modify (fun arr => arr.set! c (some r))
            return true
        | some r2 =>
            if (← dfs r2 adj visRef matchRef) then
              matchRef.modify (fun arr => arr.set! c (some r))
              return true
            else
              loop (i+1)
    else
      return false
  loop 0

partial def solveCase (pts : Array (Nat × Nat)) : IO Nat := do
  let mut adj : Array (Array Nat) := Array.replicate 120 #[]
  for (x,y) in pts do
    adj := adj.set! x ((adj[x]!).push y)
  let matchRef ← IO.mkRef (Array.replicate 120 (Option.none : Option Nat))
  let mut res := 0
  for r in [0:120] do
    let visRef ← IO.mkRef (Array.replicate 120 false)
    if (← dfs r adj visRef matchRef) then
      res := res + 1
  pure res

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut pts : Array (Nat × Nat) := #[]
    for _ in [0:n] do
      let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
      let x := parts[0]! |>.toNat!
      let y := parts[1]! |>.toNat!
      pts := pts.push (x, y)
    let ans ← solveCase pts
    IO.println ans
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
