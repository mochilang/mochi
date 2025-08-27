/- Solution for SPOJ IGARB - Ignore the Garbage
https://www.spoj.com/problems/IGARB/
-/

import Std
open Std

private def digitsFirst : Array Nat := #[1,2,5,6,8,9]
private def digitsAll   : Array Nat := #[0,1,2,5,6,8,9]

private def rot (d : Nat) : Nat :=
  match d with
  | 0 => 0
  | 1 => 1
  | 2 => 2
  | 5 => 5
  | 6 => 9
  | 8 => 8
  | 9 => 6
  | _ => 0

private def digitsToString (ds : List Nat) : String :=
  ds.foldl (fun acc d => acc.push (Char.ofNat (d + 48))) ""

-- find length and 0-based index within that length
private partial def findLen (k : Nat) : Nat × Nat :=
  let rec loop (len k : Nat) : Nat × Nat :=
    let cnt := 6 * Nat.pow 7 (len - 1)
    if k <= cnt then (len, k-1)
    else loop (len + 1) (k - cnt)
  loop 1 k

private def buildDigits (len k : Nat) : List Nat :=
  Id.run do
    let mut k := k
    let mut pow := Nat.pow 7 (len - 1)
    let mut arr : Array Nat := Array.mkEmpty len
    let firstIdx := k / pow
    arr := arr.push (digitsFirst[firstIdx]!)
    k := k % pow
    for _ in [1:len] do
      pow := pow / 7
      let idx := k / pow
      arr := arr.push (digitsAll[idx]!)
      k := k % pow
    return arr.toList

private def rotateDigits (ds : List Nat) : List Nat :=
  ds.foldl (fun acc d => rot d :: acc) []

private def solve (k : Nat) : String :=
  let (len, idx) := findLen k
  let ds := buildDigits len idx
  let rd := rotateDigits ds
  digitsToString rd

private partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let k := toks[idx]!.toNat!
    let res := solve k
    process toks (idx + 1) (t - 1) (res :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    let outs := process toks 1 t []
    for line in outs do
      IO.println line
