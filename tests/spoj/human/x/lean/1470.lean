-- https://www.spoj.com/problems/SEQ2/

import Std
open Std

def modifySame (seq : List Int) (i t : Nat) (c : Int) : List Int :=
  let pre := seq.take (i - 1)
  let suf := seq.drop (i - 1 + t)
  pre ++ List.replicate t c ++ suf

def insertSeq (seq : List Int) (i : Nat) (s : List Int) : List Int :=
  let pre := seq.take i
  let suf := seq.drop i
  pre ++ s ++ suf

def deleteSeq (seq : List Int) (i t : Nat) : List Int :=
  let pre := seq.take (i - 1)
  let suf := seq.drop (i - 1 + t)
  pre ++ suf

def reverseRange (seq : List Int) (i t : Nat) : List Int :=
  let pre := seq.take (i - 1)
  let mid := (seq.drop (i - 1)).take t
  let suf := seq.drop (i - 1 + t)
  pre ++ mid.reverse ++ suf

def rangeSum (seq : List Int) (i t : Nat) : Int :=
  let mid := (seq.drop (i - 1)).take t
  mid.foldl (· + ·) 0

def maxSubArray (seq : List Int) : Int :=
  match seq with
  | [] => 0
  | h :: t =>
      let rec aux (best curr : Int) (xs : List Int) :=
        match xs with
        | [] => best
        | x :: xs =>
            let curr' := max (curr + x) x
            let best' := max best curr'
            aux best' curr' xs
      aux h h t

def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  return ((s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun x => x ≠ "")).toArray

def main : IO Unit := do
  let tokens ← readTokens
  let idxRef ← IO.mkRef 0
  let next : IO String := do
    let i ← idxRef.get
    idxRef.set (i+1)
    pure <| tokens[i]!
  let nextNat : IO Nat := do
    let s ← next
    match s.toNat? with
    | some v => pure v
    | none => pure 0
  let nextInt : IO Int := do
    let s ← next
    match s.toInt? with
    | some v => pure v
    | none => pure 0

  let tCases ← nextNat
  for _case in [0:tCases] do
    let n ← nextNat
    let m ← nextNat
    let mut seq : List Int := []
    for _ in [0:n] do
      let x ← nextInt
      seq := x :: seq
    seq := seq.reverse

    for _ in [0:m] do
      let op ← next
      match op with
      | "MAKE-SAME" =>
          let i ← nextNat
          let t ← nextNat
          let c ← nextInt
          seq := modifySame seq i t c
      | "INSERT" =>
          let i ← nextNat
          let t ← nextNat
          let mut ins : List Int := []
          for _ in [0:t] do
            let x ← nextInt
            ins := x :: ins
          ins := ins.reverse
          seq := insertSeq seq i ins
      | "DELETE" =>
          let i ← nextNat
          let t ← nextNat
          seq := deleteSeq seq i t
      | "REVERSE" =>
          let i ← nextNat
          let t ← nextNat
          seq := reverseRange seq i t
      | "GET-SUM" =>
          let i ← nextNat
          let t ← nextNat
          let s := rangeSum seq i t
          IO.println s
      | "MAX-SUM" =>
          IO.println (maxSubArray seq)
      | _ => pure ()
