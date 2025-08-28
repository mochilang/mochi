/- Solution for SPOJ APRIME - Anti-prime Sequences
https://www.spoj.com/problems/APRIME/
-/
import Std
open Std

partial def isPrime (n : Nat) : Bool :=
  if n < 2 then
    false
  else
    let rec loop (i : Nat) : Bool :=
      if i * i > n then true
      else if n % i == 0 then false
      else loop (i + 1)
    loop 2

partial def valid (d : Nat) (prefix : List Nat) (x : Nat) : Bool :=
  let rec aux (ps : List Nat) (sum len : Nat) : Bool :=
    if len >= d then true
    else
      match ps with
      | [] => true
      | h :: t =>
        let sum := sum + h
        let len := len + 1
        if len >= 2 && isPrime sum then false
        else aux t sum len
  aux prefix x 1

partial def dfs (nums : List Nat) (used : Std.HashSet Nat)
    (prefix : List Nat) (d : Nat) : Option (List Nat) :=
  if prefix.length == nums.length then
    some prefix.reverse
  else
    let rec search : List Nat -> Option (List Nat)
    | [] => none
    | x :: xs =>
      if used.contains x then
        search xs
      else if valid d prefix x then
        match dfs nums (used.insert x) (x :: prefix) d with
        | some sol => some sol
        | none => search xs
      else
        search xs
    search nums

def solve (n m d : Nat) : Option (List Nat) :=
  let nums := (List.range (m + 1)).drop n
  dfs nums (Std.HashSet.empty) [] d

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let s := line.trim
  if s.isEmpty then
    loop h
  else
    let parts := s.split (· = ' ') |>.filter (· ≠ "")
    let n := parts.get! 0 |>.toNat!
    let m := parts.get! 1 |>.toNat!
    let d := parts.get! 2 |>.toNat!
    if n == 0 && m == 0 && d == 0 then
      pure ()
    else
      match solve n m d with
      | some seq =>
        let strs := seq.map (fun x => s!"{x}")
        let out := String.intercalate "," strs
        IO.println out
      | none => IO.println "No anti-prime sequence exists."
      loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
