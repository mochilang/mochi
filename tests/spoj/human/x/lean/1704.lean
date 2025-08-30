/- Solution for SPOJ CDOWN - Countdown (descendants)
https://www.spoj.com/problems/CDOWN/
-/
import Std
open Std

/-- Count descendants exactly `depth` generations below `start`. -/
partial def countDescendants (children : Std.HashMap String (List String)) (start : String) (depth : Nat) : Nat :=
  let rec bfs (q : List (String × Nat)) (cnt : Nat) : Nat :=
    match q with
    | [] => cnt
    | (name, d) :: rest =>
      if d == depth then
        bfs rest (cnt + 1)
      else if d < depth then
        let kids := children.findD name []
        bfs (rest ++ kids.map (fun c => (c, d + 1))) cnt
      else
        bfs rest cnt
  bfs [(start, 0)] 0

/-- Parse all whitespace separated tokens from stdin. -/
def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let mut toks : Array String := #[]
  while !(← h.isEof) do
    let line ← h.getLine
    for w in line.split (· = ' ') do
      let s := w.trim
      if s ≠ "" then
        toks := toks.push s
  return toks

/-- Solve problem given all tokens. -/
def solve (toks : Array String) : IO Unit := do
  let t := toks.get! 0 |>.toNat!
  let mut i := 1
  for caseNo in [1:t+1] do
    let n := toks.get! i |>.toNat!; i := i + 1
    let d := toks.get! i |>.toNat!; i := i + 1
    let mut ch : Std.HashMap String (List String) := {}
    for _ in [0:n] do
      let name := toks.get! i; i := i + 1
      let m := toks.get! i |>.toNat!; i := i + 1
      let mut kids : List String := []
      for _ in [0:m] do
        let c := toks.get! i; i := i + 1
        kids := c :: kids
      ch := ch.insert name kids.reverse
    let mut res : List (String × Nat) := []
    for (name, _) in ch.toList do
      let c := countDescendants ch name d
      if c > 0 then
        res := (name, c) :: res
    let res := res.qsort (fun a b => if a.snd == b.snd then a.fst < b.fst else b.snd < a.snd)
    let out :=
      if res.length ≤ 3 then res
      else
        let thr := (res.get! 2).snd
        res.filter (fun p => p.snd ≥ thr)
    IO.println s!"Tree {caseNo}:"
    for (name, c) in out do
      IO.println s!"{name} {c}"
    IO.println ""


def main : IO Unit := do
  let toks ← readTokens
  solve toks
