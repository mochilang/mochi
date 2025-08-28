/- Solution for SPOJ LSORT - Sorting is not easy
https://www.spoj.com/problems/LSORT/
-/
import Std
open Std

private def solveCase (n : Nat) (perm : Array Nat) : Nat := Id.run do
  -- positions
  let mut pos := Array.mkArray (n+1) 0
  for i in [0:n] do
    let v := perm.get! i
    pos := pos.set! v (i+1)
  -- less[v][i]
  let mut less := Array.mkArray (n+1) (Array.mkArray (n+1) 0)
  for v in [1:n+1] do
    let mut row := less.get! v
    for i in [1:n+1] do
      let prev := row.get! (i-1)
      let add := if pos.get! i < pos.get! v then 1 else 0
      row := row.set! i (prev + add)
    less := less.set! v row
  -- dp[l][r]
  let mut dp := Array.mkArray (n+2) (Array.mkArray (n+2) 0)
  for i in [1:n+1] do
    dp := dp.modify i (fun row => row.set! i (pos.get! i))
  for len in [2:n+1] do
    for l in [1:n - len + 2] do
      let r := l + len - 1
      let m := len
      let rankL := pos.get! l - ((less.get! l).get! r - (less.get! l).get! l)
      let rankR := pos.get! r - ((less.get! r).get! (r-1) - (less.get! r).get! (l-1))
      let costL := (dp.get! (l+1)).get! r + rankL * m
      let costR := (dp.get! l).get! (r-1) + rankR * m
      let best := if costL < costR then costL else costR
      dp := dp.modify l (fun row => row.set! r best)
  return (dp.get! 1).get! n

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let nLine ← h.getLine
    if nLine.trim == "" then
      loop h t
    else
      let n := nLine.trim.toNat!
      let line ← h.getLine
      let arr := line.trim.split (· = ' ') |>.filter (· ≠ "") |>.map String.toNat!
      IO.println (solveCase n arr.toArray)
      loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
