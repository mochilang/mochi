/- Solution for SPOJ ORDERS - Orders
https://www.spoj.com/problems/ORDERS/
-/
import Std
open Std

/-- least significant set bit --/
def lowbit (x : Nat) : Nat :=
  x - Nat.land x (x - 1)

/-- add `delta` at index `idx` in Fenwick tree `bit` of size `n` --/
def bitUpdate (bit : Array Int) (n idx : Nat) (delta : Int) : Array Int :=
  Id.run do
    let mut b := bit
    let mut i := idx
    while _h : i <= n do
      b := b.set! i (b[i]! + delta)
      let lb := lowbit i
      i := i + lb
    pure b

/-- prefix sum query for Fenwick tree --/
def bitQuery (bit : Array Int) (idx : Nat) : Int :=
  Id.run do
    let mut s : Int := 0
    let mut i := idx
    while _h : i > 0 do
      s := s + bit[i]!
      let lb := lowbit i
      i := i - lb
    pure s

/-- find smallest index with prefix sum ≥ k --/
def bitFindKth (bit : Array Int) (n k : Nat) : Nat :=
  let rec go (l r : Nat) : Nat :=
    if l < r then
      let m := (l + r) / 2
      if bitQuery bit m < Int.ofNat k then go (m+1) r else go l m
    else l
  go 1 n

/-- reconstruct initial ranks from inversion counts --/
def reconstruct (w : Array Nat) : Array Nat :=
  let n := w.size
  Id.run do
    -- initialize Fenwick tree with all ones
    let mut bit : Array Int := Array.replicate (n+1) 0
    let mut j := 1
    while _h : j <= n do
      bit := bitUpdate bit n j 1
      j := j + 1
    -- process positions from right to left
    let mut res : Array Nat := Array.mkArray n 0
    let mut i := n
    while _h : i > 0 do
      let idx := i - 1
      let k := i - w[idx]!
      let pos := bitFindKth bit n k
      res := res.set! idx pos
      bit := bitUpdate bit n pos (-1)
      i := i - 1
    pure res

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let line := (← h.getLine).trim
    let parts := (line.split (· = ' ')).filter (· ≠ "")
    let w := (parts.map (fun s => s.toNat!)).toArray
    let arr := reconstruct w
    let out := String.intercalate " " (arr.toList.map (fun x => toString x))
    IO.println out
    solve h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
