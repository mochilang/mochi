/- Solution for SPOJ FRACTION - Sort fractions
https://www.spoj.com/problems/FRACTION/
-/
import Std
open Std

/-- generate the first `maxIdx` terms of the Farey sequence of order `n` --/
partial def farey (n maxIdx : Nat) : Array (Nat × Nat) :=
  if maxIdx = 0 then #[] else
    let rec go (a b c d : Nat) (seq : Array (Nat × Nat)) (count : Nat) : Array (Nat × Nat) :=
      if count >= maxIdx then seq
      else
        let seq := seq.push (c, d)
        if c = 1 && d = 1 then seq
        else
          let k := (n + b) / d
          let e := k * c - a
          let f := k * d - b
          go c d e f seq (count + 1)
    go 0 1 1 n #[(0,1)] 1

/-- read all natural numbers from stdin --/
def readNats : IO (Array Nat) := do
  let stdin ← IO.getStdin
  let s ← stdin.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

def main : IO Unit := do
  let data ← readNats
  if data.size = 0 then
    return
  let t := data[0]!
  let mut idx := 1
  for _ in [0:t] do
    let n := data[idx]!; let m := data[idx+1]!; idx := idx + 2
    let mut qs : Array Nat := Array.mkEmpty m
    for _ in [0:m] do
      qs := qs.push data[idx]!; idx := idx + 1
    let maxIdx := qs.foldl (fun acc x => Nat.max acc x) 0
    let seq := farey n maxIdx
    for q in qs do
      let (a,b) := seq[q-1]!
      IO.println s!"{a}/{b}"
