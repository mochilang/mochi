/- Solution for SPOJ HAN01 - Ha-noi!
https://www.spoj.com/problems/HAN01/
-/

import Std
open Std

partial def solveRec (n k src dst aux : Nat) (pos : Array Nat) : Array Nat :=
  match n with
  | 0 => pos
  | nPlusOne =>
    let n' := nPlusOne - 1
    let m := Nat.pow 2 n' - 1
    if k <= m then
      let pos := pos.set! nPlusOne src
      solveRec n' k src aux dst pos
    else
      let pos := pos.set! nPlusOne dst
      if k == m + 1 then
        solveRec n' m src aux dst pos
      else
        solveRec n' (k - (m + 1)) aux dst src pos

def solveCase (n k : Nat) : List String :=
  let pos := solveRec n k 1 2 3 (Array.mkArray (n + 1) 0)
  let mut towers : Array (List Nat) := #[[], [], []]
  for d in List.range n do
    let disk := d + 1
    let peg := pos.get! disk
    let idx := peg - 1
    towers := towers.set! idx (disk :: towers.get! idx)
  let mut lines := []
  for i in [1, 2, 3] do
    let ds := towers.get! (i - 1)
    if ds.isEmpty then
      lines := lines.concat s!"{i}:"
    else
      let nums := String.intercalate "|" (ds.map toString)
      lines := lines.concat s!"{i}: {nums}"
  lines

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit :=
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let parts := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let n := parts.get! 0 |>.toNat!
    let k := parts.get! 1 |>.toNat!
    for l in solveCase n k do
      IO.println l
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
