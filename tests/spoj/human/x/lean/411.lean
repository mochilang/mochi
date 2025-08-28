/- Solution for SPOJ NUMQDW - Number of quite different words
https://www.spoj.com/problems/NUMQDW/
-/

import Std
open Std

def MOD : Nat := 4242

-- Build predecessor mask for each letter
private def buildPred (c : Nat) (w : String) : Array Nat :=
  Id.run do
    let mut pred : Array Nat := Array.mkArray c 0
    let mut seen : Nat := 0
    for ch in w.data do
      let idx := ch.toNat - 'A'.toNat
      pred := pred.set! idx ((pred.get! idx) ||| seen)
      seen := seen ||| (1 <<< idx)
    return pred

-- Build transition matrix between subsets of used letters
private def buildTrans (c : Nat) (pred : Array Nat) : Array (Array Nat) :=
  Id.run do
    let size := 1 <<< c
    let mut m : Array (Array Nat) := Array.mkArray size (Array.mkArray size 0)
    for s in [0:size] do
      let mut row := m.get! s
      for y in [0:c] do
        if (s &&& pred.get! y) == 0 then
          let t := s ||| (1 <<< y)
          row := row.set! t ((row.get! t + 1) % MOD)
      m := m.set! s row
    return m

-- multiply row vector by matrix
private def vecMulMat (v : Array Nat) (m : Array (Array Nat)) : Array Nat :=
  Id.run do
    let n := v.size
    let mut res : Array Nat := Array.mkArray n 0
    for j in [0:n] do
      let mut s := 0
      for k in [0:n] do
        s := (s + v.get! k * (m.get! k).get! j) % MOD
      res := res.set! j s
    return res

-- matrix multiplication
private def matMul (a b : Array (Array Nat)) : Array (Array Nat) :=
  Id.run do
    let n := a.size
    let mut res : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    for i in [0:n] do
      for j in [0:n] do
        let mut s := 0
        for k in [0:n] do
          s := (s + (a.get! i).get! k * (b.get! k).get! j) % MOD
        res := res.modify i (fun row => row.set! j s)
    return res

-- multiply vector by matrix^p
private def vecPow (v : Array Nat) (m : Array (Array Nat)) (p0 : Nat) : Array Nat :=
  Id.run do
    let mut res := v
    let mut base := m
    let mut p := p0
    while p > 0 do
      if p % 2 == 1 then
        res := vecMulMat res base
      base := matMul base base
      p := p / 2
    return res

private def solveCase (n c : Nat) (w : String) : Nat :=
  let pred := buildPred c w
  let trans := buildTrans c pred
  let size := 1 <<< c
  let mut vec : Array Nat := Array.mkArray size 0
  vec := vec.set! 0 1
  let res := vecPow vec trans n
  res.foldl (fun acc x => (acc + x) % MOD) 0

private def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut outs : Array String := #[]
    for _ in [0:t] do
      let n := toks.get! idx |>.toNat!
      idx := idx + 1
      let c := toks.get! idx |>.toNat!
      idx := idx + 1
      let w := toks.get! idx
      idx := idx + 1
      let ans := solveCase n c w
      outs := outs.push (toString ans)
    return outs

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |> Array.ofList
  let outs := solve toks
  for line in outs do
    IO.println line
