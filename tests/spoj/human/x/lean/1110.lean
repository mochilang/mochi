/- Solution for SPOJ Sudoku - 16x16 Sudoku
https://www.spoj.com/problems/SUDOKU/
-/

import Std
open Std

def allMask : Nat := (1 <<< 16) - 1

partial def solveBoard (init : Array (Array Nat)) : Array (Array Nat) :=
  Id.run do
    let mut board := init
    let mut rowMask := Array.mkArray 16 0
    let mut colMask := Array.mkArray 16 0
    let mut boxMask := Array.mkArray 16 0
    let mut empty : Array (Nat × Nat) := #[]
    -- initialize masks and empty cells
    for r in [0:16] do
      for c in [0:16] do
        let v := board.get! r |>.get! c
        if v = 16 then
          empty := empty.push (r, c)
        else
          let bit := (1 <<< v)
          rowMask := rowMask.set! r ((rowMask.get! r) ||| bit)
          colMask := colMask.set! c ((colMask.get! c) ||| bit)
          let b := (r / 4) * 4 + c / 4
          boxMask := boxMask.set! b ((boxMask.get! b) ||| bit)
    
    partial def dfs (idx : Nat) : Bool := do
      if idx = empty.size then
        true
      else
        -- choose cell with minimum candidates
        let mut bestIdx := idx
        let mut bestMask := allMask
        let mut bestCnt := 17
        let mut fail := false
        for j in [idx:empty.size] do
          let (r,c) := empty.get! j
          let b := (r / 4) * 4 + c / 4
          let used := rowMask.get! r ||| colMask.get! c ||| boxMask.get! b
          let mask := allMask ^^^ used
          let mut cnt := 0
          for v in [0:16] do
            if (mask &&& (1 <<< v)) ≠ 0 then
              cnt := cnt + 1
          if cnt = 0 then
            fail := true
          else if cnt < bestCnt then
            bestCnt := cnt
            bestIdx := j
            bestMask := mask
        if fail then
          false
        else
          if bestIdx ≠ idx then
            let tmp := empty.get! idx
            empty := empty.set! idx (empty.get! bestIdx)
            empty := empty.set! bestIdx tmp
          let (r,c) := empty.get! idx
          let b := (r / 4) * 4 + c / 4
          let mask := bestMask
          let rec tryVal (v : Nat) : Bool :=
            if v = 16 then
              false
            else
              let bit := (1 <<< v)
              if (mask &&& bit) ≠ 0 then
                let row := board.get! r
                board := board.set! r (row.set! c v)
                rowMask := rowMask.set! r ((rowMask.get! r) ||| bit)
                colMask := colMask.set! c ((colMask.get! c) ||| bit)
                boxMask := boxMask.set! b ((boxMask.get! b) ||| bit)
                if dfs (idx + 1) then
                  true
                else
                  rowMask := rowMask.set! r ((rowMask.get! r) &&& (allMask ^^^ bit))
                  colMask := colMask.set! c ((colMask.get! c) &&& (allMask ^^^ bit))
                  boxMask := boxMask.set! b ((boxMask.get! b) &&& (allMask ^^^ bit))
                  tryVal (v + 1)
              else
                tryVal (v + 1)
          tryVal 0
    let _ := dfs 0
    board

partial def readBoards (lines : Array String) (t idx : Nat) (outLines : Array String) : IO (Array String) := do
  if t = 0 then
    pure outLines
  else
    let mut board := Array.mkArray 16 (Array.mkArray 16 16)
    let mut i := idx
    for r in [0:16] do
      let line := lines.get! i
      let chars := line.data.toArray
      let mut row := Array.mkArray 16 16
      for c in [0:16] do
        let ch := chars.get! c
        if ch = '-' then
          row := row.set! c 16
        else
          row := row.set! c (ch.toNat - 'A'.toNat)
      board := board.set! r row
      i := i + 1
    if i < lines.size && (lines.get! i).trim = "" then
      i := i + 1
    let solved := solveBoard board
    let mut outLines := outLines
    for r in [0:16] do
      let row := solved.get! r
      let mut s := ""
      for v in row do
        let ch := Char.ofNat ('A'.toNat + v)
        s := s.push ch
      outLines := outLines.push s
    if t > 1 then
      outLines := outLines.push ""
    readBoards lines (t - 1) i outLines

def main : IO Unit := do
  let data ← IO.readStdin
  let lines := data.splitOn "\n" |>.toArray
  let k := (lines.get! 0).trim.toNat!
  let res ← readBoards lines k 1 #[]
  IO.println (String.intercalate "\n" res.toList)
