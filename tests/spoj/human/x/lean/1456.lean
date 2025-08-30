/- Solution for SPOJ LOSTCT - The Secret of a Lost City
https://www.spoj.com/problems/LOSTCT/
-/

import Std
open Std

abbrev PosMap := Std.HashMap Nat Nat

/-- insert k -> v keeping minimal value --/
def insertMin (m : PosMap) (k v : Nat) : PosMap :=
  match m.find? k with
  | some old => if v < old then m.insert k v else m
  | none => m.insert k v

partial def parseNP (s : String) (nouns advs : Array String)
    (cache : IO.Ref (Std.HashMap Nat PosMap)) (i : Nat) : IO PosMap := do
  let mp ← cache.get
  match mp.find? i with
  | some r => return r
  | none =>
      let mut res : PosMap := Std.HashMap.empty
      for w in nouns do
        if (s.drop i).startsWith w then
          res := insertMin res (i + w.length) 1
      for a in advs do
        if (s.drop i).startsWith a then
          let m ← parseNP s nouns advs cache (i + a.length)
          for (j,c) in m.toList do
            res := insertMin res j (c + 1)
      cache.modify (fun mp => mp.insert i res)
      return res

partial def parseVP (s : String) (verbs advs : Array String)
    (cache : IO.Ref (Std.HashMap Nat PosMap)) (i : Nat) : IO PosMap := do
  let mp ← cache.get
  match mp.find? i with
  | some r => return r
  | none =>
      let mut res : PosMap := Std.HashMap.empty
      for w in verbs do
        if (s.drop i).startsWith w then
          res := insertMin res (i + w.length) 1
      for a in advs do
        if (s.drop i).startsWith a then
          let m ← parseVP s verbs advs cache (i + a.length)
          for (j,c) in m.toList do
            res := insertMin res j (c + 1)
      cache.modify (fun mp => mp.insert i res)
      return res

partial def tailPart (s : String) (nouns verbs advs : Array String)
    (npRef vpRef tailRef : IO.Ref (Std.HashMap Nat PosMap)) (i : Nat) : IO PosMap := do
  let mp ← tailRef.get
  match mp.find? i with
  | some r => return r
  | none =>
      let mut res : PosMap := Std.HashMap.empty
      res := res.insert i 0
      let vpMap ← parseVP s verbs advs vpRef i
      for (jVP,wVP) in vpMap.toList do
        res := insertMin res jVP wVP
        let npMap ← parseNP s nouns advs npRef jVP
        for (jNP,wNP) in npMap.toList do
          let tailMap ← tailPart s nouns verbs advs npRef vpRef tailRef jNP
          for (jEnd,wTail) in tailMap.toList do
            res := insertMin res jEnd (wVP + wNP + wTail)
      tailRef.modify (fun mp => mp.insert i res)
      return res

partial def sentencePart (s : String) (nouns verbs advs : Array String)
    (npRef vpRef tailRef sentRef : IO.Ref (Std.HashMap Nat PosMap)) (i : Nat) : IO PosMap := do
  let mp ← sentRef.get
  match mp.find? i with
  | some r => return r
  | none =>
      let npMap ← parseNP s nouns advs npRef i
      let mut res : PosMap := Std.HashMap.empty
      for (jNP,wNP) in npMap.toList do
        let tailMap ← tailPart s nouns verbs advs npRef vpRef tailRef jNP
        for (jEnd,wTail) in tailMap.toList do
          res := insertMin res jEnd (wNP + wTail)
      sentRef.modify (fun mp => mp.insert i res)
      return res

def better (a b : Nat × Nat) : Bool :=
  a.fst < b.fst || (a.fst == b.fst && a.snd < b.snd)

partial def solve (s : String) (nouns verbs advs : Array String)
    (npRef vpRef tailRef sentRef : IO.Ref (Std.HashMap Nat PosMap))
    (dpRef : IO.Ref (Std.HashMap Nat (Nat × Nat))) (i : Nat) : IO (Nat × Nat) := do
  let len := s.length
  if i == len then
    return (0,0)
  let mp ← dpRef.get
  match mp.find? i with
  | some v => return v
  | none =>
      let sentMap ← sentencePart s nouns verbs advs npRef vpRef tailRef sentRef i
      let mut best? : Option (Nat × Nat) := none
      for (j,wSent) in sentMap.toList do
        let (sRest,wRest) ← solve s nouns verbs advs npRef vpRef tailRef sentRef dpRef j
        let cand := (sRest + 1, wRest + wSent)
        best? := some (match best? with
                       | none => cand
                       | some b => if better cand b then cand else b)
      let best := match best? with
                  | some v => v
                  | none => (0,0)
      dpRef.modify (fun m => m.insert i best)
      return best

partial def run (s : String) (nouns verbs advs : Array String) : IO Unit := do
  let npRef ← IO.mkRef (Std.HashMap.empty : Std.HashMap Nat PosMap)
  let vpRef ← IO.mkRef (Std.HashMap.empty : Std.HashMap Nat PosMap)
  let tailRef ← IO.mkRef (Std.HashMap.empty : Std.HashMap Nat PosMap)
  let sentRef ← IO.mkRef (Std.HashMap.empty : Std.HashMap Nat PosMap)
  let dpRef ← IO.mkRef (Std.HashMap.empty : Std.HashMap Nat (Nat × Nat))
  let (sc,wc) ← solve s nouns verbs advs npRef vpRef tailRef sentRef dpRef 0
  IO.println sc
  IO.println wc

def main : IO Unit := do
  let h ← IO.getStdin
  let n := (← h.getLine).trim.toNat!
  let mut nouns : Array String := #[]
  let mut verbs : Array String := #[]
  let mut advs : Array String := #[]
  for _ in [0:n] do
    let line := (← h.getLine).trim
    let word := line.drop 2
    let c := line.data.get! 0
    if c = 'n' then
      nouns := nouns.push word
    else if c = 'v' then
      verbs := verbs.push word
    else
      advs := advs.push word
  let strLine := (← h.getLine).trim
  let s := strLine.dropRight 1
  run s nouns verbs advs
