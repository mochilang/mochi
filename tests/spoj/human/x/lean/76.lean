/- Solution for SPOJ EDIT2 - Editor Inverse
https://www.spoj.com/problems/EDIT2/
-/

import Std
open Std

-- helper to check if halves of substring are equal
partial def halvesEqual (s : Array Char) (start mid fin : Nat) : Bool :=
  let mut ok := true
  let len := mid - start
  for i in [0:len] do
    if s[start+i]! != s[mid+i]! then
      ok := false
  ok

-- build program for substring using cost and dcnt matrices
partial def buildProgram (chars : Array Char) (cost dcnt : Array (Array Nat)) (a b : Nat) : String :=
  if a >= b then
    ""
  else
    let rowA := cost[a]!
    let rowD := dcnt[a]!
    let costAB := rowA[b]!
    let costAB1 := rowA[b-1]!
    let dAB := rowD[b]!
    let dAB1 := rowD[b-1]!
    if costAB == costAB1 + 1 && dAB == dAB1 then
      buildProgram chars cost dcnt a (b-1) ++ String.singleton (chars[b-1]!)
    else
      let mid := a + (b - a) / 2
      buildProgram chars cost dcnt a mid ++ "d"

-- lexicographic comparison for lists of Nat
partial def lessLex (a b : List Nat) : Bool :=
  match a, b with
  | [], [] => False
  | [], _ => True
  | _, [] => False
  | x::xs, y::ys => if x = y then lessLex xs ys else x < y

partial def solveLine (s : String) : (Nat × List String) := Id.run do
  let chars := s.data.toArray
  let n := chars.size
  -- cost and d-count matrices
  let mut cost := Array.replicate (n+1) (Array.replicate (n+1) 0)
  let mut dcnt := Array.replicate (n+1) (Array.replicate (n+1) 0)
  for len in [1:n+1] do
    let mut start := 0
    while start + len <= n do
      let fin := start + len
      let row := cost[start]!
      let drow := dcnt[start]!
      let mut bestCost := row[fin-1]! + 1
      let mut bestD := drow[fin-1]!
      if len % 2 == 0 then
        let mid := start + len/2
        if halvesEqual chars start mid fin then
          let candCost := cost[start]![mid]! + 1
          let candD := dcnt[start]![mid]! + 1
          if candCost < bestCost || (candCost == bestCost && candD < bestD) then
            bestCost := candCost
            bestD := candD
      cost := cost.set! start (row.set! fin bestCost)
      dcnt := dcnt.set! start (drow.set! fin bestD)
      start := start + 1
  -- DP over splits
  let inf := 1000000000
  let mut dpCost := Array.replicate (n+1) inf
  let mut dpLines := Array.replicate (n+1) 0
  let mut dpPrev := Array.replicate (n+1) 0
  let mut dpLineCosts := Array.replicate (n+1) ([] : List Nat)
  dpCost := dpCost.set! 0 0
  for i in [1:n+1] do
    for j in [0:i] do
      let lineCost := cost[j]![i]!
      let candCost := dpCost[j]! + lineCost + 1
      let candLines := dpLines[j]! + 1
      let candList := dpLineCosts[j]! ++ [lineCost]
      let better :=
        if candCost < dpCost[i]! then true
        else if candCost > dpCost[i]! then false
        else if candLines < dpLines[i]! then true
        else if candLines > dpLines[i]! then false
        else lessLex candList (dpLineCosts[i]!)
      if better then
        dpCost := dpCost.set! i candCost
        dpLines := dpLines.set! i candLines
        dpPrev := dpPrev.set! i j
        dpLineCosts := dpLineCosts.set! i candList
  -- reconstruct boundaries
  let mut idx := n
  let mut bounds : List Nat := []
  while idx > 0 do
    bounds := idx :: bounds
    idx := dpPrev[idx]!
  bounds := 0 :: bounds
  -- build programs per segment
  let rec collect (bs : List Nat) (acc : List String) : List String :=
    match bs with
    | a :: b :: rest =>
        let prog := buildProgram chars cost dcnt a b
        collect (b :: rest) (acc ++ [prog])
    | _ => acc
  let programs := collect bounds []
  let totalCost := dpCost[n]! + 1
  return (totalCost, programs)

partial def process (h : IO.FS.Stream) (case : Nat) : IO Unit := do
  if case = 10 then
    pure ()
  else
    let line? ← h.getLine?
    match line? with
    | none => pure ()
    | some line =>
        let s := line.trim
        if s = "" then
          pure ()
        else
          let (c, progs) := solveLine s
          IO.println c
          for p in progs do
            IO.println p
          IO.println ""
          process h (case + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  process h 0
