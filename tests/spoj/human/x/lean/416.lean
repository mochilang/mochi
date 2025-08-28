/- Solution for SPOJ DIV15 - Divisibility by 15
https://www.spoj.com/problems/DIV15/
-/

import Std
open Std

private def digitChar (n : Nat) : Char := Char.ofNat (48 + n)

private def replicateChar (n : Nat) (c : Char) (acc : List Char) : List Char :=
  match n with
  | 0 => acc
  | Nat.succ k => replicateChar k c (c :: acc)

private def buildDigits (cnt : Array Nat) : List Char :=
  let rec loop (i : Nat) (acc : List Char) : List Char :=
    if h : i > 9 then acc
    else
      let acc := replicateChar (cnt.get! i) (digitChar i) acc
      loop (i+1) acc
  loop 0 []

private def removeOne (cnt : Array Nat) (ds : List Nat) : Option (Array Nat) :=
  match ds with
  | [] => none
  | d :: rest =>
    if cnt.get! d > 0 then
      some (cnt.set! d (cnt.get! d - 1))
    else
      removeOne cnt rest

private def removeTwo (cnt : Array Nat) (ds : List Nat) : Option (Array Nat) :=
  match removeOne cnt ds with
  | none => none
  | some c => removeOne c ds

private def adjust (cnt : Array Nat) (mod3 : Nat) : Option (Array Nat) :=
  match mod3 with
  | 0 => some cnt
  | 1 =>
    match removeOne cnt [1,4,7] with
    | some c => some c
    | none => removeTwo cnt [2,5,8]
  | 2 =>
    match removeOne cnt [2,5,8] with
    | some c => some c
    | none => removeTwo cnt [1,4,7]
  | _ => none

private def attempt (orig : Array Nat) (total : Nat) (d : Nat) : Option String :=
  if orig.get! d = 0 then none else
  let cnt := orig.set! d (orig.get! d - 1)
  match adjust cnt (total % 3) with
  | none => none
  | some c =>
    let digits := buildDigits c
    let chars := digits ++ [digitChar d]
    let trimmed := List.dropWhile (fun ch => ch = '0') chars
    let s := String.mk trimmed
    if s.length = 0 then some "0" else some s

private def best (cs : List String) : Option String :=
  match cs with
  | [] => none
  | c :: cs =>
    some <| cs.foldl (fun acc s =>
      if s.length > acc.length ∨ (s.length = acc.length ∧ s >= acc) then s else acc) c

private def solve (s : String) : String :=
  let counts := Array.mkArray 10 0
  let counts := s.data.foldl (fun a ch =>
    let d := ch.toNat - '0'.toNat
    a.set! d (a.get! d + 1)) counts
  let total := (List.range 10).foldl (fun acc i => acc + i * counts.get! i) 0
  let cands := [attempt counts total 0, attempt counts total 5].filterMap id
  match best cands with
  | none => "impossible"
  | some r => r

partial def handle (h : IO.FS.Stream) : IO Unit := do
  let tLine ← h.getLine
  let mut t := tLine.trim.toNat!
  for _ in List.range t do
    let line ← h.getLine
    IO.println (solve line.trim)

def main : IO Unit := do
  handle (← IO.getStdin)
