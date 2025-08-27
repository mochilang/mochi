/- Solution for SPOJ POKER - Poker
https://www.spoj.com/problems/POKER/
-/

import Std
open Std

def rankVal : Char -> Nat
| 'A' => 14
| 'K' => 13
| 'Q' => 12
| 'J' => 11
| 'T' => 10
| c   => c.toNat - '0'.toNat

def parseCard (s : String) : Nat × Char :=
  let cs := s.data
  (rankVal cs[0]!, cs[1]!)

def isSeq : List Nat -> Bool
| [a,b,c,d,e] => b == a+1 && c == b+1 && d == c+1 && e == d+1
| _ => false

def classify (cards : List (Nat × Char)) : String :=
  let ranks := cards.map (·.1)
  let suits := cards.map (·.2)
  let flush := suits.all (fun s => s = suits.head!)
  let sorted := ranks.toArray.qsort (·<·) |>.toList
  let straight :=
    isSeq sorted ||
      let alt := sorted.map (fun r => if r == 14 then 1 else r)
      isSeq (alt.toArray.qsort (·<·) |>.toList)
  let m := ranks.foldl
      (fun m r => match m.find? r with
                  | some v => m.insert r (v + 1)
                  | none => m.insert r 1)
      (Std.HashMap.empty : Std.HashMap Nat Nat)
  let counts := m.fold [] (fun acc _ v => v :: acc)
  let countList := counts.qsort (·>·)
  if flush && straight then
    if sorted = [10,11,12,13,14] then "royal flush" else "straight flush"
  else if countList = [4,1] then "four of a kind"
  else if countList = [3,2] then "full house"
  else if flush then "flush"
  else if straight then "straight"
  else if countList = [3,1,1] then "three of a kind"
  else if countList = [2,2,1] then "two pairs"
  else if countList = [2,1,1,1] then "pair"
  else "high card"

partial def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := tLine.trim.toNat!
  for _ in [0:t] do
    let line ← h.getLine
    let parts := line.trim.split (· == ' ')
    let cards := parts.map parseCard
    IO.println (classify cards)
