/- Solution for SPOJ LEONARDO - Leonardo Notebook
https://www.spoj.com/problems/LEONARDO/
-/

import Std
open Std

def parsePerm (s : String) : List Nat :=
  s.data.map (fun c => c.toNat - 'A'.toNat)

partial def cycleLen (perm : List Nat) (i : Nat) (vis : List Nat) : Nat × List Nat :=
  if vis.contains i then (0, vis)
  else
    let vis := i :: vis
    let (l, vis) := cycleLen perm (perm.get! i) vis
    (l + 1, vis)

partial def gather (perm : List Nat) (i : Nat) (vis : List Nat) (acc : List Nat) : List Nat :=
  if h : i < 26 then
    have : Decidable (i < 26) := inferInstance
    if vis.contains i then gather perm (i+1) vis acc
    else
      let (l, vis) := cycleLen perm i vis
      gather perm (i+1) vis (l :: acc)
  else acc

def isSquare (perm : List Nat) : Bool :=
  let lens := gather perm 0 [] []
  let counts := lens.foldl (fun m l =>
    if l % 2 == 0 then m.insert l ((m.findD l 0) + 1) else m) (Std.HashMap.empty)
  counts.toList.all (fun ⟨_, c⟩ => c % 2 == 0)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let perm := parsePerm line.trim
    let res := if isSquare perm then "Yes" else "No"
    IO.println res
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
