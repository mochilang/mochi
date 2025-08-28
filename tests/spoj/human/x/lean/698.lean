/- Solution for SPOJ PLHOP - Plane Hopping
https://www.spoj.com/problems/PLHOP/
-/

import Std
open Std

def INF : Nat := (1 <<< 60)

def matMul (a b : Array (Array Nat)) (n : Nat) : Array (Array Nat) :=
  Id.run do
    let mut res : Array (Array Nat) := Array.replicate n (Array.replicate n INF)
    for i in [0:n] do
      for j in [0:n] do
        let mut best := INF
        for k in [0:n] do
          let v := a[i]![k]! + b[k]![j]!
          if v < best then
            best := v
        res := res.modify i (fun row => row.set! j best)
    return res

def matPow (a : Array (Array Nat)) (k n : Nat) : Array (Array Nat) :=
  Id.run do
    let mut result : Array (Array Nat) := Array.replicate n (Array.replicate n INF)
    for i in [0:n] do
      result := result.modify i (fun row => row.set! i 0)
    let mut base := a
    let mut p := k
    while p > 0 do
      if p % 2 == 1 then
        result := matMul result base n
      base := matMul base base n
      p := p / 2
    return result

def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks[0]!.toNat!
    let mut idx := 1
    let mut outs : Array String := #[]
    for case in [0:t] do
      let k := toks[idx]!.toNat!
      let n := toks[idx+1]!.toNat!
      idx := idx + 2
      let mut mat : Array (Array Nat) := #[]
      for _ in [0:n] do
        let mut row : Array Nat := #[]
        for _ in [0:n] do
          row := row.push (toks[idx]!.toNat!)
          idx := idx + 1
        mat := mat.push row
      let ans := matPow mat k n
      outs := outs.push s!"Region #{case+1}:"
      for i in [0:n] do
        let row := ans[i]!
        let line := String.intercalate " " (row.toList.map toString)
        outs := outs.push line
      outs := outs.push ""
    return outs

def main : IO Unit := do
  let handle ← IO.getStdin
  let data ← handle.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let res := solve toks
  for line in res do
    IO.println line
