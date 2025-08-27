/- Solution for SPOJ POSTERS - Election Posters
https://www.spoj.com/problems/POSTERS/
-/

import Std
open Std

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, parent') := find parent px
    (r, parent'.set! x r)

partial def paint (parent : Array Nat) (l r : Nat) : (Bool × Array Nat) :=
  let (j, parent0) := find parent l
  if j >= r then
    (false, parent0)
  else
    let rec loop (parent : Array Nat) (idx : Nat) (vis : Bool) : (Bool × Array Nat) :=
      if idx >= r then
        (vis, parent)
      else
        let parent := parent.set! idx (idx + 1)
        let (next, parent) := find parent (idx + 1)
        loop parent next true
    loop parent0 j false

def solve (pairs : Array (Nat × Nat)) : Nat :=
  Id.run do
    let n := pairs.size
    -- collect coordinates and transform intervals with r+1
    let mut coords : Array Nat := Array.mkEmpty (2 * n)
    let mut inter : Array (Nat × Nat) := Array.mkEmpty n
    for (l, r) in pairs do
      coords := coords.push l
      coords := coords.push (r + 1)
      inter := inter.push (l, r + 1)
    let sorted := coords.qsort (· < ·)
    let mut uniq : Array Nat := #[]
    for x in sorted do
      if uniq.isEmpty || uniq.back! != x then
        uniq := uniq.push x
    let m := uniq.size
    let mut mp : Std.HashMap Nat Nat := Std.HashMap.emptyWithCapacity 0
    for i in [0:m] do
      mp := mp.insert (uniq[i]!) i
    let mut segs : Array (Nat × Nat) := Array.mkEmpty n
    for (l, r1) in inter do
      let lIdx := (mp.get? l).get!
      let rIdx := (mp.get? r1).get!
      segs := segs.push (lIdx, rIdx)
    let mut parent : Array Nat := (List.range m).toArray
    let rec loop (i : Nat) (parent : Array Nat) (ans : Nat) : (Nat × Array Nat) :=
      if i = 0 then (ans, parent)
      else
        let idx := i - 1
        let (l, r) := segs[idx]!
        let (vis, parent) := paint parent l r
        let ans := if vis then ans + 1 else ans
        loop idx parent ans
    let (res, _) := loop n parent 0
    return res

partial def parsePairs (k : Nat) (xs : List Nat) (acc : List (Nat × Nat)) :
    (List (Nat × Nat) × List Nat) :=
  match k with
  | 0 => (acc.reverse, xs)
  | Nat.succ k' =>
    match xs with
    | l :: r :: rest => parsePairs k' rest ((l, r) :: acc)
    | _ => (acc.reverse, [])

partial def processCases : Nat → List Nat → List String → List String
| 0, _, acc => acc.reverse
| Nat.succ t, xs, acc =>
  match xs with
  | n :: rest =>
    let (pairs, rest') := parsePairs n rest []
    let ans := solve pairs.toArray
    processCases t rest' (toString ans :: acc)
  | _ => acc.reverse

def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let ints := parseInts input
  match ints with
  | [] => pure ()
  | t :: rest =>
    let lines := processCases t rest []
    IO.println (String.intercalate "\n" lines)
