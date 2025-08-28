/- Solution for SPOJ PSTRING - Remove The String
https://www.spoj.com/problems/PSTRING/
-/

import Std
open Std

private def buildLps (p : Array Char) : Array Nat :=
  let m := p.size
  let mut lps := Array.mkArray m 0
  let mut len := 0
  for i in [1:m] do
    let mut l := len
    while l > 0 && p.get! i != p.get! l do
      l := lps.get! (l - 1)
    if p.get! i == p.get! l then
      l := l + 1
    lps := lps.set! i l
    len := l
  lps

private def nextState (p : Array Char) (lps : Array Nat) (j : Nat) (c : Char) : Nat :=
  let mut k := j
  while k > 0 && p.get! k != c do
    k := lps.get! (k - 1)
  if p.get! k == c then k + 1 else 0

private def solveCase (x y : String) : Nat :=
  let t := x.data.toArray
  let p := y.data.toArray
  let m := p.size
  if m = 0 then t.size else
    let lps := buildLps p
    let inf := 1000000000
    let mut dp := Array.mkArray m inf
    dp := dp.set! 0 0
    for c in t do
      let mut nxt := Array.mkArray m inf
      for j in [0:m] do
        let del := dp.get! j + 1
        nxt := nxt.set! j (Nat.min (nxt.get! j) del)
        let ns := nextState p lps j c
        if ns < m then
          nxt := nxt.set! ns (Nat.min (nxt.get! ns) (dp.get! j))
      dp := nxt
    dp.foldl (fun a b => if a < b then a else b) inf

def main : IO Unit := do
  let data ← IO.readStdin
  let lines := data.split (fun c => c = '\n' || c = '\r') |>.filter (fun s => s.trim ≠ "")
  let arr := lines.toArray
  let mut idx := 0
  while idx + 1 < arr.size do
    let x := arr[idx]!.trim
    let y := arr[idx+1]!.trim
    IO.println (solveCase x y)
    idx := idx + 2
