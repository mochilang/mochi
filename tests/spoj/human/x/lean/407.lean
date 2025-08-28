/- Solution for SPOJ RNUMBER - Random Number
https://www.spoj.com/problems/RNUMBER/
-/

import Std
open Std

/-- Build DP table where `dp[pos][prev]` counts sequences for positions
`pos..N` given the previous value `prev`. -/
private def buildDP (M N : Nat) : Array (Array Nat) :=
  Id.run do
    let mut dp := Array.mkArray (N+2) (Array.mkArray (M+1) 0)
    -- base case: position N+1 has 1 way for any previous value
    dp := dp.set! (N+1) (Array.mkArray (M+1) 1)
    for i in [0:N] do
      let pos := N - i
      let next := dp.get! (pos + 1)
      -- cumulative sums from right to left of next row
      let mut cum := Array.mkArray (M+1) 0
      let mut s : Nat := 0
      for j in [0:M+1] do
        let v := M - j
        s := s + next.get! v
        cum := cum.set! v s
      -- current row
      let mut row := Array.mkArray (M+1) 0
      for prev in [0:M+1] do
        let minVal := if prev > pos then prev else pos
        row := row.set! prev (cum.get! minVal)
      dp := dp.set! pos row
    return dp

/-- Map binary fraction bits to integer in `[start, finish]`. -/
private def mapBinary (start finish : Nat) (bits : List Char) : Nat :=
  Id.run do
    let mut a := start
    let mut b := finish
    for c in bits do
      if a < b then
        let mid := (a + b) / 2
        if c = '0' then
          b := mid
        else
          a := mid + 1
    a

/-- Format numbers with width 3 separated into lines of at most 20 numbers. -/
private def printSeq (seq : Array Nat) : IO Unit := do
  let mut cnt := 0
  for i in [0:seq.size] do
    let num := seq.get! i
    let s := toString num
    let pad := String.mk (List.replicate (3 - s.length) ' ')
    IO.print (pad ++ s)
    cnt := cnt + 1
    let last := i == seq.size - 1
    if last || cnt % 20 == 0 then
      IO.println ""
    else
      IO.print " "

/-- Main program: process each dataset. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let cases := (tokens[0]!).toNat!
  let rec loop (idx case : Nat) : IO Unit := do
    if case == cases then
      pure ()
    else
      let M := (tokens[idx]!).toNat!
      let N := (tokens[idx+1]!).toNat!
      let bitsStr := tokens[idx+2]!
      let bits := (bitsStr.drop 2).data
      let dp := buildDP M N
      let total := (dp.get! 1).get! 0
      let mut k := mapBinary 1 total bits
      let mut prev := 0
      let mut seq : Array Nat := #[]
      for pos in [1:N+1] do
        let mut v := if prev > pos then prev else pos
        let nextRow := dp.get! (pos + 1)
        let mut done := false
        while !done && v ≤ M do
          let cnt := nextRow.get! v
          if k ≤ cnt then
            seq := seq.push v
            prev := v
            done := true
          else
            k := k - cnt
            v := v + 1
      printSeq seq
      loop (idx + 3) (case + 1)
  loop 1 0
