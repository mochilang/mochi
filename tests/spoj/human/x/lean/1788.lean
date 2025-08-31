/- Solution for SPOJ FRACTAN - Fractan
https://www.spoj.com/problems/FRACTAN/
-/

import Std
open Std

partial def isPowerOfTwo (n : Nat) : Option Nat :=
  let rec aux (x e : Nat) : Option Nat :=
    if x == 1 then some e
    else if x % 2 == 0 then aux (x / 2) (e + 1)
    else none
  aux n 0

partial def findNext (cur : Nat) : List (Nat × Nat) → Option Nat
  | [] => none
  | (num, den) :: t =>
      if (cur * num) % den == 0 then
        some ((cur * num) / den)
      else
        findNext cur t

partial def simulate (m : Nat) (cur : Nat) (fracs : List (Nat × Nat))
    (exps : List Nat) (steps : Nat) : List Nat :=
  let exps' :=
    match isPowerOfTwo cur with
    | some e => exps ++ [e]
    | none => exps
  if exps'.length >= m || steps >= 7654321 then
    exps'
  else
    match findNext cur fracs with
    | none => exps'
    | some nxt => simulate m nxt fracs exps' (steps + 1)

def runCase (m : Nat) (start : Nat) (fracs : List (Nat × Nat)) : List Nat :=
  simulate m start fracs [] 0

partial def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  return ((s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (· ≠ "")).toArray

def main : IO Unit := do
  let tokens ← readTokens
  let idxRef ← IO.mkRef 0
  let nextNat : IO Nat := do
    let i ← idxRef.get
    idxRef.set (i + 1)
    pure (tokens[i]!).toNat!
  let mut outLines : List String := []
  while true do
    let m ← nextNat
    if m == 0 then
      break
    let start ← nextNat
    let k ← nextNat
    let mut fracs : List (Nat × Nat) := []
    for _ in [0:k] do
      let num ← nextNat
      let den ← nextNat
      fracs := fracs ++ [(num, den)]
    let res := runCase m start fracs
    let line := String.intercalate " " (res.map toString)
    outLines := outLines ++ [line]
  IO.println (String.intercalate "\n" outLines)
