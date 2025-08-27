/- Solution for SPOJ PHRASES - Relevant Phrases of Annihilation
https://www.spoj.com/problems/PHRASES/
-/

import Std
open Std

/-- compute b^e in UInt64 modulo 2^64 --/
partial def pow (b : UInt64) (e : Nat) : UInt64 :=
  let rec loop (acc : UInt64) (k : Nat) :=
    match k with
    | 0 => acc
    | Nat.succ k' => loop (acc * b) k'
  loop 1 e

/-- substrings of length `L` that appear at least twice disjointly in `s` --/
partial def repeatedSubstrings (s : String) (L : Nat) : Std.HashSet UInt64 :=
  if L == 0 || L > s.length then
    {}
  else
    let bytes := s.toUTF8.data
    let base : UInt64 := 911382323
    let powL := pow base L
    let mut h : UInt64 := 0
    for j in [0:L] do
      let c := bytes.get! j
      h := h * base + UInt64.ofNat c.toNat
    let mut first : Std.HashMap UInt64 Nat := {}
    let mut res : Std.HashSet UInt64 := {}
    first := first.insert h 0
    let limit := bytes.size - L
    for i in [1:limit+1] do
      let out := UInt64.ofNat (bytes.get! (i-1)).toNat
      let inn := UInt64.ofNat (bytes.get! (i+L-1)).toNat
      h := (h - out * powL) * base + inn
      match first.find? h with
      | none => first := first.insert h i
      | some j =>
          if j + L <= i then
            res := res.insert h
    res

/-- check if a substring of length `L` exists in all strings at least twice --/
partial def existsCommon (ss : List String) (L : Nat) : Bool :=
  if L == 0 then
    true
  else
    let mut counts : Std.HashMap UInt64 Nat := {}
    for s in ss do
      let set := repeatedSubstrings s L
      for h in set.toList do
        counts := counts.insert h (counts.findD h 0 + 1)
    let need := ss.length
    let mut ok := false
    for (_k, v) in counts.toList do
      if v == need then
        ok := true
    ok

/-- compute maximum length of substring appearing twice disjointly in all strings --/
partial def maxCommon (ss : List String) : Nat :=
  if ss.isEmpty then 0 else
    let minLen := ss.foldl (fun m s => Nat.min m s.length) (ss.head!.length)
    let rec go (lo hi : Nat) :=
      if lo >= hi then lo else
        let mid := (lo + hi + 1) / 2
        if existsCommon ss mid then go mid hi else go lo (mid - 1)
    go 0 minLen

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut msgs : List String := []
    for _ in [0:n] do
      let line ← h.getLine
      msgs := msgs.append [line.trim]
    IO.println (maxCommon msgs)
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
