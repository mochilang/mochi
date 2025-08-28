/- Solution for SPOJ LAZYPROG - The lazy programmer
https://www.spoj.com/problems/LAZYPROG/
-/

import Std
open Std

structure Contract where
  a : Nat
  b : Nat
  d : Nat

def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

partial def reduceLoop (arr : Array (Nat × Float)) (i : Nat) (over cost : Float) : (Array (Nat × Float) × Float) :=
  if over ≤ 0 || i ≥ arr.size then
    (arr, cost)
  else
    match arr[i]? with
    | none => (arr, cost)
    | some (a,b) =>
        let r := Float.min b over
        let arr := arr.set! i (a, b - r)
        let cost := cost + r / Float.ofNat a
        let over := over - r
        if b - r ≤ 1e-9 then
          reduceLoop arr (i+1) over cost
        else
          reduceLoop arr i over cost

partial def reduce (arr0 : Array (Nat × Float)) (over : Float) : (Array (Nat × Float) × Float) :=
  let arr := arr0.qsort (fun x y => x.fst > y.fst)
  let (arr, cost) := reduceLoop arr 0 over 0.0
  (arr.filter (fun p => p.snd > 1e-9), cost)

partial def solveCase (cs : Array Contract) : Float := Id.run do
  let mut arr : Array (Nat × Float) := #[]
  let mut total : Float := 0.0
  let mut cost : Float := 0.0
  let sorted := cs.qsort (fun x y => x.d < y.d)
  for c in sorted do
    arr := arr.push (c.a, Float.ofNat c.b)
    total := total + Float.ofNat c.b
    let deadline := Float.ofNat c.d
    if total > deadline then
      let over := total - deadline
      let (arr', add) := reduce arr over
      arr := arr'
      cost := cost + add
      total := deadline
  return cost

partial def process (toks : Array String) (idx cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let n := (toks.get! idx).toNat!
    let mut i := idx + 1
    let mut cs : Array Contract := #[]
    for _ in [0:n] do
      let a := (toks.get! i).toNat!
      let b := (toks.get! (i+1)).toNat!
      let d := (toks.get! (i+2)).toNat!
      cs := cs.push ⟨a,b,d⟩
      i := i + 3
    let ans := solveCase cs
    IO.println (format2 ans)
    process toks i (cases - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let t := (toks.get! 0).toNat!
  process toks 1 t
