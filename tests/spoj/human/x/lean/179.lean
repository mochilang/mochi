/- Solution for SPOJ WORDEQ - Word equations
https://www.spoj.com/problems/WORDEQ/
-/

import Std
open Std

structure DSU where
  parent : IO.Ref (Array Nat)
  rank   : IO.Ref (Array Nat)

namespace DSU

partial def find (d : DSU) (x : Nat) : IO Nat := do
  let pArr ← d.parent.get
  let p := pArr.get! x
  if p = x then
    pure x
  else
    let r ← find d p
    let pArr ← d.parent.get
    d.parent.set (pArr.set! x r)
    pure r

@[inline] def union (d : DSU) (a b : Nat) : IO Unit := do
  let ra ← find d a
  let rb ← find d b
  if ra = rb then
    pure ()
  else
    let rankArr ← d.rank.get
    let pa := rankArr.get! ra
    let pb := rankArr.get! rb
    if pa < pb then
      let pArr ← d.parent.get
      d.parent.set (pArr.set! ra rb)
    else if pa > pb then
      let pArr ← d.parent.get
      d.parent.set (pArr.set! rb ra)
    else
      let pArr ← d.parent.get
      d.parent.set (pArr.set! rb ra)
      d.rank.set (rankArr.set! ra (pa + 1))

end DSU

@[inline] def DSU.mk (n : Nat) : IO DSU := do
  let parent ← IO.mkRef (Array.iota n)
  let rank ← IO.mkRef (Array.mkArray n 0)
  pure ⟨parent, rank⟩

partial def pow2 : Nat → Nat
  | 0     => 1
  | n+1 => pow2 n * 2

/-- expand a side into indices -/
private def expand (s : String) (lens offs : Array Nat) (zero one : Nat) : Array Nat :=
  Id.run do
    let arr := s.data.toArray
    let mut res : Array Nat := Array.empty
    for ch in arr do
      if ch = '0' then
        res := res.push zero
      else if ch = '1' then
        res := res.push one
      else
        let idx := ch.toNat - 'a'.toNat
        let start := offs.get! idx
        let len := lens.get! idx
        for j in [0:len] do
          res := res.push (start + j)
    return res

partial def solveOne (lens : Array Nat) (left right : String) : IO Nat := do
  -- compute offsets
  let mut offs : Array Nat := Array.mkArray 26 0
  let mut pos := 0
  for i in [0:lens.size] do
    offs := offs.set! i pos
    pos := pos + lens.get! i
  let total := pos
  let zeroIdx := total
  let oneIdx := total + 1
  let leftArr := expand left lens offs zeroIdx oneIdx
  let rightArr := expand right lens offs zeroIdx oneIdx
  if leftArr.size ≠ rightArr.size then
    return 0
  let dsu ← DSU.mk (total + 2)
  let mut ok := true
  for i in [0:leftArr.size] do
    if ok then
      let a := leftArr.get! i
      let b := rightArr.get! i
      DSU.union dsu a b
      let z ← DSU.find dsu zeroIdx
      let o ← DSU.find dsu oneIdx
      if z = o then
        ok := false
  if !ok then
    return 0
  let z ← DSU.find dsu zeroIdx
  let o ← DSU.find dsu oneIdx
  let mut seen : Std.HashSet Nat := {} -- components not fixed
  let mut cnt := 0
  for i in [0:total] do
    let r ← DSU.find dsu i
    if r ≠ z && r ≠ o && !seen.contains r then
      seen := seen.insert r
      cnt := cnt + 1
  return pow2 cnt

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut outs : List String := []
    for _ in [0:t] do
      let k := toks.get! idx |>.toNat!
      let mut lens : Array Nat := Array.mkArray 26 0
      for i in [0:k] do
        lens := lens.set! i (toks.get! (idx + 1 + i) |>.toNat!)
      let left := toks.get! (idx + 2 + k)
      let right := toks.get! (idx + 4 + k)
      let ans ← solveOne lens left right
      outs := (toString ans) :: outs
      idx := idx + 5 + k
    for line in outs.reverse do
      IO.println line
