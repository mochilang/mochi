/- Solution for SPOJ BOOKS1 - Copying Books
https://www.spoj.com/problems/BOOKS1/
-/

import Std
open Std

partial def canSplit (pages : Array Nat) (k limit : Nat) : Bool :=
  let mut cnt := 1
  let mut s := 0
  for p in pages do
    if s + p > limit then
      cnt := cnt + 1
      s := p
    else
      s := s + p
  cnt <= k

partial def assignCuts (pages : Array Nat) (k limit : Nat) : Array Bool :=
  let m := pages.size
  Id.run do
    let mut cuts := Array.mkArray m false
    let mut sum := 0
    let mut rem := k
    let mut i := m
    while h : i > 0 do
      let j := i - 1
      let p := pages.get! j
      if sum + p > limit || j + 1 < rem then
        cuts := cuts.set! j true
        sum := p
        rem := rem - 1
      else
        sum := sum + p
      i := j
    pure cuts

partial def solveCase (pages : Array Nat) (k : Nat) : String :=
  let mut lo := 0
  let mut hi := 0
  for p in pages do
    lo := Nat.max lo p
    hi := hi + p
  let mut l := lo
  let mut h := hi
  while l < h do
    let mid := (l + h) / 2
    if canSplit pages k mid then
      h := mid
    else
      l := mid + 1
  let limit := l
  let cuts := assignCuts pages k limit
  let m := pages.size
  let mut out := ""
  for i in [0:m] do
    out := out ++ toString (pages.get! i)
    if i < m - 1 then
      if cuts.get! i then
        out := out ++ " / "
      else
        out := out ++ " "
  out

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let mkLine := (← h.getLine).trim
    if mkLine.isEmpty then
      loop h t
    else
      let parts := mkLine.splitOn " "
      let m := parts.get! 0 |>.toNat!
      let k := parts.get! 1 |>.toNat!
      let pLine := (← h.getLine).trim
      let pages : Array Nat := (pLine.splitOn " ").map (·.toNat!) |>.toArray
      IO.println (solveCase pages k)
      loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
