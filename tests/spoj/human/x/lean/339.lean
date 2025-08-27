/- Solution for SPOJ SEQ - Recursive Sequence
https://www.spoj.com/problems/SEQ/
-/

import Std
open Std

def MOD : Nat := 1000000000

def matMul (a b : Array (Array Nat)) : Array (Array Nat) :=
  Id.run do
    let n := a.size
    let m := (b.get! 0).size
    let p := b.size
    let mut res : Array (Array Nat) := Array.mkArray n (Array.mkArray m 0)
    for i in [0:n] do
      for j in [0:m] do
        let mut s := 0
        for k in [0:p] do
          let x := (a.get! i).get! k
          let y := (b.get! k).get! j
          s := (s + x * y % MOD) % MOD
        res := res.modify i (fun row => row.set! j s)
    return res

def matPow (m : Array (Array Nat)) (power : Nat) : Array (Array Nat) :=
  Id.run do
    let size := m.size
    let mut result : Array (Array Nat) := Array.mkArray size (Array.mkArray size 0)
    for i in [0:size] do
      result := result.modify i (fun row => row.set! i 1)
    let mut base := m
    let mut p := power
    while p > 0 do
      if p % 2 == 1 then
        result := matMul result base
      base := matMul base base
      p := p / 2
    return result

def solveCase (k : Nat) (b c : Array Nat) (n : Nat) : Nat :=
  if n <= k then
    b.get! (n - 1) % MOD
  else
    Id.run do
      let mut T : Array (Array Nat) := #[]
      let firstRow := c.map (fun x => x % MOD)
      T := T.push firstRow
      for i in [1:k] do
        let mut row := Array.mkArray k 0
        row := row.set! (i - 1) 1
        T := T.push row
      let M := matPow T (n - k)
      let mut F : Array (Array Nat) := #[]
      for i in [0:k] do
        let val := b.get! (k - 1 - i) % MOD
        F := F.push #[val]
      let res := matMul M F
      return (res.get! 0).get! 0 % MOD

def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut outs : Array String := #[]
    for _ in [0:t] do
      let k := toks.get! idx |>.toNat!
      idx := idx + 1
      let mut b : Array Nat := #[]
      for _ in [0:k] do
        b := b.push (toks.get! idx |>.toNat!)
        idx := idx + 1
      let mut c : Array Nat := #[]
      for _ in [0:k] do
        c := c.push (toks.get! idx |>.toNat!)
        idx := idx + 1
      let n := toks.get! idx |>.toNat!
      idx := idx + 1
      let ans := solveCase k b c n
      outs := outs.push (toString ans)
    return outs

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let results := solve toks
  for line in results do
    IO.println line
