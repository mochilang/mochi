/- Solution for SPOJ BASE - Basically Speaking
https://www.spoj.com/problems/BASE/
-/
import Std
open Std

def digitVal (c : Char) : Nat :=
  let c := c.toUpper
  if '0' ≤ c ∧ c ≤ '9' then
    c.toNat - '0'.toNat
  else
    c.toNat - 'A'.toNat + 10

partial def fromBase (s : String) (b : Nat) : Nat :=
  let rec loop : List Char → Nat → Nat
    | [], acc => acc
    | c :: cs, acc => loop cs (acc * b + digitVal c)
  loop s.toList 0

partial def toBaseAux (n b : Nat) (acc : String) : String :=
  if n = 0 then acc
  else
    let d := n % b
    let c := if d < 10 then Char.ofNat (d + '0'.toNat)
             else Char.ofNat (d - 10 + 'A'.toNat)
    toBaseAux (n / b) b (String.singleton c ++ acc)

def toBase (n b : Nat) : String :=
  if n = 0 then "0" else toBaseAux n b ""

def padLeft (width : Nat) (s : String) : String :=
  if h : s.length < width then
    String.mk (List.replicate (width - s.length) ' ') ++ s
  else s

def convert (num : String) (from to : Nat) : String :=
  let val := fromBase num from
  let res := toBase val to
  if res.length > 7 then padLeft 7 "ERROR" else padLeft 7 res

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let line ← h.getLine
    let line := line.trim
    if line.isEmpty then
      process h
    else
      let parts := line.split (fun c => c = ' ' ∨ c = '\t')
                    |> List.filter (fun s => s ≠ "")
      match parts with
      | n :: a :: b :: _ =>
          let out := convert n a.toNat! b.toNat!
          IO.println out
          process h
      | _ =>
          pure ()

def main : IO Unit := do
  process (← IO.getStdin)
