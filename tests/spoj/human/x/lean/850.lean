/- Solution for SPOJ WM06 - Soccer Choreography
https://www.spoj.com/problems/WM06/
-/

import Std
open Std

def parseInts (line : String) : List Int :=
  line.trim.split (fun c => c = ' ').filterMap String.toInt?

def reverseSegment (arr : List Int) (l r : Nat) : List Int :=
  let (pre, rest) := arr.splitAt l
  let (mid, suf) := rest.splitAt (r + 1 - l)
  pre ++ (mid.reverse.map (fun x => -x)) ++ suf

def breakpoints (arr : List Int) : Nat :=
  let n := arr.length
  let ext := [0] ++ arr ++ [Int.ofNat n + 1]
  let pairs := ext.zip ext.tail
  pairs.foldl (init := 0) (fun acc ab =>
    let (a,b) := ab
    if b ≠ a + 1 then acc + 1 else acc)

partial def dfs (depth : Nat) (arr : List Int) : Option (List (Nat × Nat)) :=
  let b := breakpoints arr
  if b = 0 then some []
  else if depth = 0 || (b + 1) / 2 > depth then none
  else
    let n := arr.length
    let rec loopL (l : Nat) : Option (List (Nat × Nat)) :=
      if h : l < n then
        let rec loopR (r : Nat) : Option (List (Nat × Nat)) :=
          if h2 : r < n then
            let nxt := reverseSegment arr l r
            if breakpoints nxt < b then
              match dfs (depth - 1) nxt with
              | some ms => some ((l, r) :: ms)
              | none => loopR (r + 1)
            else
              loopR (r + 1)
          else
            loopL (l + 1)
        loopR (l + 1)
      else
        none
    loopL 0

partial def searchMoves (arr : List Int) : List (Nat × Nat) :=
  let b := breakpoints arr
  let rec aux (d : Nat) : List (Nat × Nat) :=
    match dfs d arr with
    | some ms => ms
    | none => aux (d + 1)
  aux ((b + 1) / 2)

partial def buildSeq (arr : List Int) (ms : List (Nat × Nat)) : List (List Int) :=
  match ms with
  | [] => [arr]
  | (l, r) :: rest =>
      let next := reverseSegment arr l r
      arr :: buildSeq next rest

def formatState (arr : List Int) : String :=
  String.intercalate " " (arr.map (fun x =>
    if x ≥ 0 then "+" ++ toString x else toString x))

partial def process (stdin : IO.FS.Stream) : IO Unit := do
  let line ← stdin.getLine
  let n := line.trim.toNat!
  if n = 0 then
    pure ()
  else
    let startLine ← stdin.getLine
    let targetLine ← stdin.getLine
    let start := parseInts startLine
    let target := parseInts targetLine
    -- build maps from player number to position and orientation in target
    let posMap : Std.HashMap Int Nat :=
      ((target.enum).foldl (init := {}) fun m p =>
        let (i, v) := p
        m.insert (Int.ofNat v.natAbs) (i + 1))
    let sgnMap : Std.HashMap Int Int :=
      ((target.enum).foldl (init := {}) fun m p =>
        let (_, v) := p
        let s := if v ≥ 0 then 1 else -1
        m.insert (Int.ofNat v.natAbs) s)
    let perm := start.map (fun v =>
      let s := if v ≥ 0 then 1 else -1
      let idx := posMap.find! (Int.ofNat v.natAbs)
      let ts := sgnMap.find! (Int.ofNat v.natAbs)
      s * ts * Int.ofNat idx)
    let moves := searchMoves perm
    let seq := buildSeq start moves
    IO.println s!"{moves.length} Steps"
    for st in seq do
      IO.println (formatState st)
    process stdin

def main : IO Unit := do
  process (← IO.getStdin)
