/- Solution for SPOJ STPAR - Street Parade
https://www.spoj.com/problems/STPAR/
-/

import Std
open Std

private def parseInts (line : String) : List Nat :=
  line.trim.split (· = ' ') |>.filterMap (fun s =>
    if s.isEmpty then none else s.toNat?)

private def canArrange (arr : List Nat) : Bool :=
  let rec process (rest : List Nat) (stack : List Nat) (expected : Nat) : Bool :=
    match rest with
    | [] =>
      let rec flush (stk : List Nat) (e : Nat) : Bool :=
        match stk with
        | [] => true
        | h :: t => if h == e then flush t (e + 1) else false
      flush stack expected
    | a :: t =>
      let rec pop (stk : List Nat) (e : Nat) : List Nat × Nat :=
        match stk with
        | h :: s => if h == e then pop s (e + 1) else (stk, e)
        | [] => ([], e)
      let (stk', e') := pop stack expected
      if a == e' then
        process t stk' (e' + 1)
      else
        process t (a :: stk') e'
  process arr [] 1

partial def loopCases (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let arr := parseInts (← h.getLine)
    IO.println (if canArrange arr then "yes" else "no")
    loopCases h

def main : IO Unit := do
  loopCases (← IO.getStdin)
