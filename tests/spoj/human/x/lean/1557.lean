/- Solution for SPOJ GSS2 - Can you answer these queries II
https://www.spoj.com/problems/GSS2/
-/

import Std
open Std

private def maxDistinctSubarray (arr : Array Int) (l r : Nat) : Int := Id.run do
  let mut last : Std.HashMap Int Nat := {}
  let mut start := l
  let mut cur : Int := 0
  let mut best : Int := 0
  let mut j := l
  while j ≤ r do
    let x := arr[j]!
    match last.find? x with
    | some idx =>
        if idx ≥ start then
          let mut k := start
          while k ≤ idx do
            let y := arr[k]!
            last := last.erase y
            cur := cur - y
            k := k + 1
          start := idx + 1
    | none => pure ()
    last := last.insert x j
    cur := cur + x
    if cur > best then best := cur
    j := j + 1
  return best

partial def process (toks : Array String) (idx q : Nat) (arr : Array Int) : IO Unit := do
  if q = 0 then
    pure ()
  else
    let l := toks[idx]!.toNat!
    let r := toks[idx+1]!.toNat!
    let ans := maxDistinctSubarray arr (l-1) (r-1)
    IO.println ans
    process toks (idx+2) (q-1) arr

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    return ()
  let n := toks[0]!.toNat!
  let mut arr : Array Int := Array.mkEmpty n
  let mut i := 0
  let mut id := 1
  while i < n do
    arr := arr.push (toks[id]!.toInt!)
    i := i + 1
    id := id + 1
  let q := toks[id]!.toNat!
  process toks (id+1) q arr
