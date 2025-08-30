/- Solution for SPOJ LOGIC2 - Logic II
https://www.spoj.com/problems/LOGIC2/
-/

import Std
open Std

inductive PropStmt where
| Is (person : Nat) (guilty : Bool)
| Day (day : Nat)

deriving Inhabited

structure Sentence where
  speaker : Nat
  prop : PropStmt

def evalSentence (g day mask : Nat) (s : Sentence) : Bool :=
  let truth :=
    match s.prop with
    | PropStmt.Is p b =>
        if b then decide (p = g) else decide (p ≠ g)
    | PropStmt.Day d => decide (d = day)
  let liar := Nat.testBit mask s.speaker
  if liar then not truth else truth

partial def bitcount (n : Nat) : Nat :=
  if n = 0 then 0 else
    (n % 2) + bitcount (n / 2)

-- Enumerate all possible guilty persons consistent with statements
partial def solveCase (names : Array String) (N : Nat) (sentences : Array Sentence) : String :=
  let M := names.size
  let masks := (List.range (Nat.shiftLeft 1 M)).filter (fun m => bitcount m = N)
  let days := List.range 7
  let candidates := (List.range M).filter (fun g =>
    masks.any (fun mask =>
      days.any (fun day =>
        sentences.all (fun s => evalSentence g day mask s))))
  match candidates with
  | [g] => names.get! g
  | [] => "Impossible"
  | _ => "Cannot Determine"

-- Find index of a name
def nameIndex (names : Array String) (name : String) : Option Nat :=
  names.findIdx? (· = name)

-- Parse a single line into a useful sentence if possible
def parseSentence (names : Array String) (line : String) : Option Sentence :=
  match line.splitOn ":" with
  | [spk, stmt] =>
      match nameIndex names spk.trim with
      | none => none
      | some speaker =>
          let stmt := stmt.trim
          if stmt == "I am guilty." then
            some { speaker := speaker, prop := PropStmt.Is speaker true }
          else if stmt == "I am not guilty." then
            some { speaker := speaker, prop := PropStmt.Is speaker false }
          else if stmt.getLast? = some '.' then
            let core := stmt.take (stmt.length - 1)
            let parts := core.splitOn " "
            match parts with
            | ["Today", "is", day] =>
                let days := #["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
                match days.findIdx? (· = day) with
                | some d => some { speaker := speaker, prop := PropStmt.Day d }
                | none => none
            | [name, "is", "guilty"] =>
                match nameIndex names name with
                | some p => some { speaker := speaker, prop := PropStmt.Is p true }
                | none => none
            | [name, "is", "not", "guilty"] =>
                match nameIndex names name with
                | some p => some { speaker := speaker, prop := PropStmt.Is p false }
                | none => none
            | _ => none
          else
            none
  | _ => none

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let header := (← h.getLine).trim
    if header = "" then
      process h
    else
      let nums := (header.split (· = ' ') |>.filter (· ≠ "")).toArray
      let M := (nums.get! 0).toNat!
      let N := (nums.get! 1).toNat!
      let P := (nums.get! 2).toNat!
      let mut names : Array String := Array.mkEmpty M
      for _ in [0:M] do
        names := names.push (← h.getLine).trim
      let mut sentences : Array Sentence := Array.mkEmpty P
      for _ in [0:P] do
        let line := (← h.getLine).trim
        match parseSentence names line with
        | some s => sentences := sentences.push s
        | none => pure ()
      IO.println (solveCase names N sentences)
      process h

def main : IO Unit := do
  process (← IO.getStdin)
