/- Solution for SPOJ FREQUENT - Frequent values
https://www.spoj.com/problems/FREQUENT/
-/
import Std
open Std

/-- size of segment tree leaves, power of two ≥ 100000 -/
def segSize : Nat := 131072

/-- build a segment tree for range maximum queries -/
partial def buildTree (vals : Array Nat) : Array Nat := Id.run do
  let mut t := Array.mkArray (2 * segSize) 0
  for i in [0:vals.size] do
    t := t.set! (segSize + i) (vals.get! i)
  let mut i := segSize - 1
  while h : i > 0 do
    let left := t.get! (i * 2)
    let right := t.get! (i * 2 + 1)
    t := t.set! i (Nat.max left right)
    i := i - 1
  return t

/-- query maximum on interval [l,r] using the segment tree `t` -/
partial def segQuery (t : Array Nat) (l r : Nat) : Nat := Id.run do
  let mut l := l + segSize
  let mut r := r + segSize
  let mut res := 0
  while l <= r do
    if l % 2 = 1 then
      res := Nat.max res (t.get! l)
      l := l + 1
    if r % 2 = 0 then
      res := Nat.max res (t.get! r)
      r := r - 1
    l := l / 2
    r := r / 2
  return res

/-- preprocess array into block ids and segment tree -/
partial def preprocess (a : Array Int) :
    (Array Nat) × (Array Nat) × (Array Nat) × Array Nat × Array Nat := Id.run do
  let n := a.size
  let mut blockId := Array.mkArray n 0
  let mut begins : Array Nat := #[]
  let mut ends : Array Nat := #[]
  let mut counts : Array Nat := #[]
  let mut i := 0
  let mut b := 0
  while i < n do
    let start := i
    let v := a.get! i
    let mut j := i
    while j < n && a.get! j = v do
      blockId := blockId.set! j b
      j := j + 1
    let fin := j - 1
    begins := begins.push start
    ends := ends.push fin
    counts := counts.push (fin - start + 1)
    i := j
    b := b + 1
  let tree := buildTree counts
  return (blockId, begins, ends, counts, tree)

/-- solve using token array starting at index `idx` -/
partial def solve (toks : Array String) (idx : Nat) (out : Array String)
    : Array String :=
  if h : idx < toks.size then
    let n := (toks.get! idx).toNat!
    if n = 0 then
      out
    else
      let q := (toks.get! (idx+1)).toNat!
      let mut arr : Array Int := Array.mkArray n 0
      for i in [0:n] do
        arr := arr.set! i ((toks.get! (idx+2+i)).toInt!)
      let (blockId, begins, ends, _counts, tree) := preprocess arr
      let mut res := out
      let mut base := idx + 2 + n
      for _ in [0:q] do
        let l := (toks.get! base).toNat! - 1
        let r := (toks.get! (base+1)).toNat! - 1
        let bL := blockId.get! l
        let bR := blockId.get! r
        let ans :=
          if bL = bR then
            r - l + 1
          else
            let leftPart := (ends.get! bL) - l + 1
            let rightPart := r - (begins.get! bR) + 1
            let mid :=
              if bL + 1 ≤ bR - 1 then segQuery tree (bL + 1) (bR - 1) else 0
            Nat.max leftPart (Nat.max rightPart mid)
        res := res.push (toString ans)
        base := base + 2
      solve toks base res
  else
    out

/-- main: read tokens, process until sentinel 0 -/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> Array.ofList
  let outs := solve toks 0 #[]
  for line in outs do
    IO.println line
