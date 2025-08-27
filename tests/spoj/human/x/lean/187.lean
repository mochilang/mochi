/- Solution for SPOJ FLBRKLIN - Flat broken lines
https://www.spoj.com/problems/FLBRKLIN/
-/

import Std
open Std

structure Pt where
  u : Int
  v : Int

partial def lowerBound (a : Array Int) (x : Int) : Nat :=
  let rec go (l r : Nat) : Nat :=
    if h : l < r then
      let m := (l + r) / 2
      if a.get! m < x then go (m + 1) r else go l m
    else
      l
  go 0 a.size

def ldsLen (vs : Array Int) : Nat :=
  let arr := vs.map (fun v => -v)
  let mut tails : Array Int := #[]
  for i in [0:arr.size] do
    let x := arr.get! i
    let pos := lowerBound tails x
    if pos == tails.size then
      tails := tails.push x
    else
      tails := tails.set! pos x
  tails.size

partial def solveCases : Nat -> List Int -> List String -> List String
| 0, _, acc => acc.reverse
| Nat.succ k, tokens, acc =>
    match tokens with
    | [] => acc.reverse
    | n :: rest =>
        let nNat := Int.toNat n
        let rec takePts (m : Nat) (xs : List Int) (accP : List Pt) : (List Pt × List Int) :=
          match m, xs with
          | 0, xs => (accP.reverse, xs)
          | Nat.succ m', x :: y :: xs' =>
              let pt : Pt := {u := x + y, v := x - y}
              takePts m' xs' (pt :: accP)
          | _, xs => (accP.reverse, xs)
        let (pts, rest2) := takePts nNat rest []
        let arr : Array Pt := pts.toArray.qsort (fun a b => if a.u == b.u then a.v > b.v else a.u < b.u)
        let vs : Array Int := arr.map (fun p => p.v)
        let res := ldsLen vs
        solveCases k rest2 (toString res :: acc)


def parseInts (s : String) : List Int :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
    |>.filterMap (fun t => if t.isEmpty then none else t.toInt?)


def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let toks := parseInts input
  match toks with
  | [] => pure ()
  | t :: rest =>
      let lines := solveCases (Int.toNat t) rest []
      IO.println (String.intercalate "\n" lines)
