/- Solution for SPOJ DFLOOR - Dance Floor
https://www.spoj.com/problems/DFLOOR/
-/

import Std
open Std

-- parse a row of bits into a UInt32 mask
private def parseRow (s : String) : UInt32 :=
  Id.run do
    let arr := s.data.toArray
    let mut mask : UInt32 := 0
    for i in [0:arr.size] do
      if arr[i]! = '1' then
        mask := mask ||| (UInt32.ofNat (1 <<< i))
    return mask

-- count set bits in a UInt32
private partial def popcount (x : UInt32) : Nat :=
  if x = 0 then 0 else popcount (x &&& (x - 1)) + 1

-- core solver: returns minimal press count and press mask for each row
private def solve (w h : Nat) (board : Array UInt32) : Option (Nat × Array UInt32) :=
  let mask : UInt32 := UInt32.ofNat ((1 <<< w) - 1)
  Id.run do
    let limit := (1 <<< w)
    let mut bestCnt := (h+1)*(w+1)
    let mut bestPress : Array UInt32 := Array.mkArray h 0
    let mut found := false
    for first in [0:limit] do
      let mut press := Array.mkArray h 0
      press := press.set! 0 (UInt32.ofNat first)
      for r in [1:h] do
        let prev := press[r-1]!
        let mut state := board[r-1]!
        state := state ^^^ prev
        state := state ^^^ ((prev <<< 1) &&& mask)
        state := state ^^^ (prev >>> 1)
        if r > 1 then
          state := state ^^^ press[r-2]!
        let need := (~~~state) &&& mask
        press := press.set! r need
      let r := h - 1
      let mut st := board[r]!
      let p := press[r]!
      st := st ^^^ p
      st := st ^^^ ((p <<< 1) &&& mask)
      st := st ^^^ (p >>> 1)
      if h > 1 then
        st := st ^^^ press[r-1]!
      if st = mask then
        let mut cnt := 0
        for r in [0:h] do
          cnt := cnt + popcount (press[r]!)
        if !found || cnt < bestCnt then
          bestCnt := cnt
          bestPress := press
          found := true
    return if found then some (bestCnt, bestPress) else none

-- convert press masks into output lines
private def movesFromPress (w h : Nat) (press : Array UInt32) : List String :=
  Id.run do
    let mut ls : List String := []
    for r in [0:h] do
      let row := press[r]!
      for c in [0:w] do
        if ((row >>> c) &&& (1:UInt32)) = 1 then
          ls := s!"{c+1} {r+1}" :: ls
    return ls.reverse

-- parse all cases and produce output lines
partial def parseAll (toks : Array String) (idx : Nat) (acc : List String) :
    List String :=
  if idx >= toks.size then acc.reverse else
    let w := toks[idx]!.toNat!
    let h := toks[idx+1]!.toNat!
    if w = 0 && h = 0 then acc.reverse else
      let mut board : Array UInt32 := Array.mkArray h 0
      for r in [0:h] do
        let rowStr := toks[idx + 2 + r]!
        board := board.set! r (parseRow rowStr)
      let res := solve w h board
      let lines :=
        match res with
        | none => ["-1"]
        | some (cnt, press) =>
            let mv := movesFromPress w h press
            (toString cnt) :: mv
      parseAll toks (idx + 2 + h) (lines.reverse ++ acc)

-- main entry point
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let lines := parseAll toks 0 []
    for line in lines do
      IO.println line
