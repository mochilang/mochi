/- Solution for SPOJ DNA - DNA Translation
https://www.spoj.com/problems/DNA/
-/

import Std
open Std

def mapStr (f : Char -> Char) (s : String) : String :=
  String.mk (s.toList.map f)

def revStr (s : String) : String :=
  String.mk (s.toList.reverse)

def complement (s : String) : String :=
  mapStr (fun c => match c with
    | 'A' => 'T'
    | 'T' => 'A'
    | 'C' => 'G'
    | 'G' => 'C'
    | _   => c) s

def toRNA (s : String) : String :=
  mapStr (fun c => if c = 'T' then 'U' else c) s

private def codonPairs : List (String × String) := [
  ("UUU","Phe"), ("UUC","Phe"),
  ("UUA","Leu"), ("UUG","Leu"),
  ("UCU","Ser"), ("UCC","Ser"), ("UCA","Ser"), ("UCG","Ser"),
  ("UAU","Tyr"), ("UAC","Tyr"),
  ("UGU","Cys"), ("UGC","Cys"), ("UGG","Trp"),
  ("CUU","Leu"), ("CUC","Leu"), ("CUA","Leu"), ("CUG","Leu"),
  ("CCU","Pro"), ("CCC","Pro"), ("CCA","Pro"), ("CCG","Pro"),
  ("CAU","His"), ("CAC","His"), ("CAA","Gln"), ("CAG","Gln"),
  ("CGU","Arg"), ("CGC","Arg"), ("CGA","Arg"), ("CGG","Arg"),
  ("AUU","Ile"), ("AUC","Ile"), ("AUA","Ile"), ("AUG","Met"),
  ("ACU","Thr"), ("ACC","Thr"), ("ACA","Thr"), ("ACG","Thr"),
  ("AAU","Asn"), ("AAC","Asn"), ("AAA","Lys"), ("AAG","Lys"),
  ("AGU","Ser"), ("AGC","Ser"), ("AGA","Arg"), ("AGG","Arg"),
  ("GUU","Val"), ("GUC","Val"), ("GUA","Val"), ("GUG","Val"),
  ("GCU","Ala"), ("GCC","Ala"), ("GCA","Ala"), ("GCG","Ala"),
  ("GAU","Asp"), ("GAC","Asp"), ("GAA","Glu"), ("GAG","Glu"),
  ("GGU","Gly"), ("GGC","Gly"), ("GGA","Gly"), ("GGG","Gly")
]

def codonMap : Std.HashMap String String :=
  Id.run do
    let mut m : Std.HashMap String String := {}
    for (k,v) in codonPairs do
      m := m.insert k v
    return m

partial def translate (rna : String) : Option String :=
  let n := rna.length
  let rec search (i : Nat) : Option String :=
    if h : i + 2 < n then
      let cod := rna.extract i (i+3)
      if cod = "AUG" then
        parse (i+3) []
      else
        search (i+1)
    else none
  and parse (j : Nat) (acc : List String) : Option String :=
    if h : j + 2 < n then
      let cod := rna.extract j (j+3)
      if cod = "UAA" || cod = "UAG" || cod = "UGA" then
        if acc.isEmpty then none else some (String.intercalate "-" acc.reverse)
      else
        match codonMap.find? cod with
        | some aa => parse (j+3) (aa :: acc)
        | none => none
    else none
  search 0

def candidates (dna : String) : List String :=
  let rev := revStr dna
  let comp := complement dna
  let revComp := revStr comp
  [toRNA dna, toRNA rev, toRNA comp, toRNA revComp]

def solve (dna : String) : String :=
  let rec loop : List String -> String
    | [] => "*** No translatable DNA found ***"
    | s :: rest =>
        match translate s with
        | some res => res
        | none => loop rest
  loop (candidates dna)

partial def process (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let s := line.trim
  if s = "*" then
    pure ()
  else
    IO.println (solve s)
    process h

def main : IO Unit := do
  process (← IO.getStdin)
