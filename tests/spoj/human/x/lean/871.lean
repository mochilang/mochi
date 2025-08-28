/- Solution for SPOJ SEQUENCE - Letter Sequence Analysis
https://www.spoj.com/problems/SEQUENCE/
-/

import Std
open Std

-- remove non-letter characters and convert to uppercase
def clean (s : String) : String :=
  let chars := s.toList.filterMap (fun c => if c.isAlpha then some c.toUpper else none)
  String.mk chars

-- collect frequencies of all substrings of length k
def collectFreq (text : String) (k : Nat) : Std.HashMap String Nat :=
  Id.run do
    let n := text.length
    let mut m : Std.HashMap String Nat := {}
    for i in [0:n] do
      if i + k ≤ n then
        let seq := text.extract i (i + k)
        let cnt := (m.findD seq 0) + 1
        m := m.insert seq cnt
    return m

-- group sequences by frequency and keep only the top five frequency groups
def topGroups (m : Std.HashMap String Nat) : Array (Nat × Array String) :=
  Id.run do
    let mut freqMap : Std.HashMap Nat (Array String) := {}
    for (seq, cnt) in m.toArray do
      let arr := (freqMap.findD cnt #[]).push seq
      freqMap := freqMap.insert cnt arr
    let arr := freqMap.toArray.map (fun (f, seqs) => (f, seqs.qsort (· < ·)))
    let sorted := arr.qsort (fun a b => a.fst > b.fst)
    let limit := if sorted.size < 5 then sorted.size else 5
    return sorted.extract 0 limit

-- format the report lines for a given sequence length
def formatReport (k : Nat) (groups : Array (Nat × Array String)) : Array String :=
  Id.run do
    let header := s!"Analysis for Letter Sequences of Length {k}"
    let dash := "-----------------------------------------"
    let mut lines : Array String := #[header, dash]
    for (freq, seqs) in groups do
      let seqStr := "(" ++ String.intercalate "," seqs.toList ++ ")"
      lines := lines.push s!"Frequency = {freq}, Sequence(s) = {seqStr}"
    lines := lines.push ""
    return lines

def main : IO Unit := do
  let input ← IO.readStdin
  let text := clean input
  let mut all : Array String := #[]
  for k in [1:6] do
    let freq := collectFreq text k
    let groups := topGroups freq
    all := all.append (formatReport k groups)
  IO.println (String.intercalate "\n" all)
