/- Solution for SPOJ AMBM - Ambitious Manager
https://www.spoj.com/problems/AMBM/
-/

import Std
import Std.Data.HashMap
open Std

-- compute the adjusted salaries b_i
@[inline] def buildB (as : Array Nat) : Array Nat :=
  Id.run do
    let mut res : Array Nat := #[]
    let mut prev : Nat := 0
    for i in [0:as.size] do
      let b := as[i]! + (prev <<< 1)
      res := res.push b
      prev := b
    return res

-- convert a bitmask into 1-based day indices
@[inline] def indicesFromMask (mask start len : Nat) : List Nat :=
  Id.run do
    let mut out : List Nat := []
    for i in [0:len] do
      if (mask &&& (1 <<< i)) ≠ 0 then
        out := (start + i + 1) :: out
    return out.reverse

-- solve a single test case
partial def solveCase (n k : Nat) (as : Array Nat) : String :=
  let b := buildB as
  let m := k / 2
  let first := b.extract 0 m
  let second := b.extract m k
  -- enumerate subset sums for first half
  let map := Id.run do
    let mut mp : Std.HashMap Nat Nat := {}
    let limit := 1 <<< first.size
    for mask in [0:limit] do
      let mut s := 0
      for i in [0:first.size] do
        if (mask &&& (1 <<< i)) ≠ 0 then
          s := s + first[i]!
      if mp.contains s then
        ()
      else
        mp := mp.insert s mask
    return mp
  -- recursively explore subsets of second half
  let rec search (idx sum mask : Nat) : Option (Nat × Nat) :=
    if idx == second.size then
      if sum ≤ n then
        match map.get? (n - sum) with
        | some m1 => some (m1, mask)
        | none => none
      else
        none
    else
      let val := second[idx]!
      if sum + val ≤ n then
        match search (idx+1) (sum + val) (mask ||| (1 <<< idx)) with
        | some r => some r
        | none => search (idx+1) sum mask
      else
        search (idx+1) sum mask
  match search 0 0 0 with
  | some (m1, m2) =>
      let idxs := (indicesFromMask m1 0 first.size) ++ (indicesFromMask m2 first.size second.size)
      String.intercalate " " (idxs.map toString)
  | none => "-1"

-- process all test cases from the integer list
partial def processCases : Nat -> List Nat -> List String -> List String
| 0, _, acc => acc.reverse
| Nat.succ t, xs, acc =>
  match xs with
  | n :: k :: rest =>
      let as := rest.take k
      let rest' := rest.drop k
      let arr := List.toArray as
      let ans := solveCase n k arr
      processCases t rest' (ans :: acc)
  | _ => acc.reverse

@[inline] def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let nums := parseInts input
  match nums with
  | [] => pure ()
  | t :: rest =>
      let outputs := processCases t rest []
      IO.println (String.intercalate "\n" outputs)
