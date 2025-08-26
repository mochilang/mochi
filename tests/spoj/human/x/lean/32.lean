/- Solution for SPOJ NHAY - A Needle in the Haystack
https://www.spoj.com/problems/NHAY/
-/

import Std
open Std

/-- Build the longest prefix suffix (LPS) table for KMP algorithm. -/
private def buildLps (pat : Array Char) : Array Nat :=
  let m := pat.size
  let rec loop (i len : Nat) (lps : Array Nat) : Array Nat :=
    if i ≥ m then lps
    else if pat.get! i == pat.get! len then
      let len := len + 1
      let lps := lps.set! i len
      loop (i + 1) len lps
    else if len ≠ 0 then
      let len := lps.get! (len - 1)
      loop i len lps
    else
      let lps := lps.set! i 0
      loop (i + 1) len lps
  loop 1 0 (Array.mkArray m 0)

/-- KMP search returning starting positions of pattern occurrences. -/
private def kmp (text pat : String) : List Nat :=
  let t := text.data.toArray
  let p := pat.data.toArray
  let n := t.size
  let m := p.size
  let lps := buildLps p
  let rec search (i j : Nat) (acc : List Nat) : List Nat :=
    if i ≥ n then acc.reverse
    else if t.get! i == p.get! j then
      let i := i + 1
      let j := j + 1
      if j == m then
        let acc := (i - j) :: acc
        search i (lps.get! (j - 1)) acc
      else
        search i j acc
    else if j ≠ 0 then
      search i (lps.get! (j - 1)) acc
    else
      search (i + 1) j acc
  search 0 0 []

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let nLine ← h.getLine
    if nLine.trim.isEmpty then
      process h
    else
      let _ := nLine.trim.toNat! -- length is ignored after validation
      let needle := (← h.getLine).trim
      let haystack := (← h.getLine).trim
      for idx in kmp haystack needle do
        IO.println idx
      IO.println ""
      process h

def main : IO Unit :=
  process (← IO.getStdin)

