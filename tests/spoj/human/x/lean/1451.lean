/- Solution for SPOJ SEQ1 - 01 Sequence
https://www.spoj.com/problems/SEQ1/
-/
import Std
open Std

partial def build (n A0 B0 L0 A1 B1 L1 : Nat) : Option String :=
  let countSuffix (arr : Array Char) (len : Nat) (ch : Char) : Nat :=
    let start := arr.size - len
    (List.range len).foldl (fun acc i => if arr.get! (start + i) == ch then acc + 1 else acc) 0
  let check (arr : Array Char) : Bool :=
    let i := arr.size
    let len0 := Nat.min L0 i
    let zeros := countSuffix arr len0 '0'
    if zeros > B0 then false
    else if i >= L0 && zeros < A0 then false
    else
      let len1 := Nat.min L1 i
      let ones := countSuffix arr len1 '1'
      if ones > B1 then false
      else if i >= L1 && ones < A1 then false
      else true
  let rec dfs (arr : Array Char) : Option (Array Char) :=
    if arr.size == n then
      some arr
    else
      let rec loop (cs : List Char) : Option (Array Char) :=
        match cs with
        | [] => none
        | c :: cs' =>
          let arr' := arr.push c
          if check arr' then
            match dfs arr' with
            | some res => some res
            | none => loop cs'
          else
            loop cs'
      loop ['0', '1']
  match dfs Array.empty with
  | some arr => some (String.mk arr.toList)
  | none => none

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let mut idx := 0
  for _ in [0:5] do
    let n := toks.get! idx |>.toNat!
    let A0 := toks.get! (idx+1) |>.toNat!
    let B0 := toks.get! (idx+2) |>.toNat!
    let L0 := toks.get! (idx+3) |>.toNat!
    let A1 := toks.get! (idx+4) |>.toNat!
    let B1 := toks.get! (idx+5) |>.toNat!
    let L1 := toks.get! (idx+6) |>.toNat!
    idx := idx + 7
    let out := match build n A0 B0 L0 A1 B1 L1 with
               | some s => s
               | none => "-1"
    IO.println out
