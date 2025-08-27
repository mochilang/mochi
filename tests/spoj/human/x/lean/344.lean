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
  | '9' => 9
  | '8' => 8
  | '7' => 7
  | '6' => 6
  | '5' => 5
  | '4' => 4
  | '3' => 3
  | '2' => 2
  | _   => 0

def isStraight (sorted : List Nat) : Bool :=
  if sorted = [2,3,4,5,14] then
    True
  else
    let rec aux : List Nat -> Bool
      | [] => True
      | [_] => True
      | a :: b :: t => b = a + 1 && aux (b :: t)
    aux sorted

def rankCounts (ranks : List Nat) : List Nat :=
  let arr := Id.run do
    let mut a := Array.mkArray 15 0
    for r in ranks do
      a := a.modify r (· + 1)
    pure a
  arr.data.filter (· > 0)

def classify (cards : List String) : String :=
  let ranks := cards.map (fun c => rankVal (c.get! 0))
  let suits := cards.map (fun c => c.get! 1)
  let flush := suits.all (fun s => s = suits.head!)
  let sorted := ranks.qsort (· < ·)
  let straight := isStraight sorted
  let cnts := (rankCounts ranks).qsort (· > ·)
  if straight && flush then
    if sorted = [10,11,12,13,14] then "royal flush" else "straight flush"
  else if cnts = [4,1] then "four of a kind"
  else if cnts = [3,2] then "full house"
  else if flush then "flush"
  else if straight then "straight"
  else if cnts = [3,1,1] then "three of a kind"
  else if cnts = [2,2,1] then "two pairs"
  else if cnts = [2,1,1,1] then "pair"
  else "high card"

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    let cards := line.trim.split (· = ' ') |>.filter (· ≠ "")
    IO.println (classify cards)
    loop h (n-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
