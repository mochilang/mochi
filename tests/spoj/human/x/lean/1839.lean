/- Solution for SPOJ BOOKCASE - The Bookcase
https://www.spoj.com/problems/BOOKCASE/
-/

import Std
open Std

private def INF : Nat := 1000000000

private def updateMin (m : Std.HashMap (Nat×Nat) Nat) (k : Nat×Nat) (v : Nat)
    : Std.HashMap (Nat×Nat) Nat :=
  let old := m.findD k INF
  if v < old then m.insert k v else m

private def solveCase (books : Array (Nat×Nat)) : Nat := Id.run do
  let sorted := books.qsort (fun a b => a.fst > b.fst)
  let totalW := books.foldl (fun acc b => acc + b.snd) 0
  let mut dp : Array (Std.HashMap (Nat×Nat) Nat) :=
    Array.mkArray 8 Std.HashMap.empty
  dp := dp.set! 0 ((Std.HashMap.empty).insert (0,0) 0)
  for book in sorted do
    let h := book.fst
    let t := book.snd
    let mut next : Array (Std.HashMap (Nat×Nat) Nat) :=
      Array.mkArray 8 Std.HashMap.empty
    for mask in [0:8] do
      let entries := (dp.get! mask).toList
      for kv in entries do
        let (p, hsum) := kv
        let w1 := p.fst
        let w2 := p.snd
        let w1' := w1 + t
        if w1' ≤ totalW then
          let mask1 := mask ||| 1
          let hsum1 := hsum + (if (mask &&& 1) = 0 then h else 0)
          let m1 := next.get! mask1
          next := next.set! mask1 (updateMin m1 (w1', w2) hsum1)
        let w2' := w2 + t
        if w2' ≤ totalW then
          let mask2 := mask ||| 2
          let hsum2 := hsum + (if (mask &&& 2) = 0 then h else 0)
          let m2 := next.get! mask2
          next := next.set! mask2 (updateMin m2 (w1, w2') hsum2)
        let mask3 := mask ||| 4
        let hsum3 := hsum + (if (mask &&& 4) = 0 then h else 0)
        let m3 := next.get! mask3
        next := next.set! mask3 (updateMin m3 (w1, w2) hsum3)
    dp := next
  let final := dp.get! 7
  let mut ans := INF
  for kv in final.toList do
    let (p, hsum) := kv
    let w1 := p.fst
    let w2 := p.snd
    if w1 + w2 ≤ totalW then
      let w3 := totalW - w1 - w2
      let width := Nat.max w1 (Nat.max w2 w3)
      let area := hsum * width
      if area < ans then ans := area
  return ans

partial def solveAll (toks : Array String) (idx cases : Nat)
    (acc : List String) : List String :=
  if cases = 0 then acc.reverse
  else
    let n := toks[idx]!.toNat!
    let mut books : Array (Nat×Nat) := Array.mkArray n (0,0)
    let mut j := idx + 1
    for i in [0:n] do
      let h := toks[j]!.toNat!
      let t := toks[j+1]!.toNat!
      books := books.set! i (h,t)
      j := j + 2
    let ans := solveCase books
    solveAll toks j (cases-1) (toString ans :: acc)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
