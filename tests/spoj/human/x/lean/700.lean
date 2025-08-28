/- Solution for SPOJ BPRED - Branch Prediction
https://www.spoj.com/problems/BPRED/
-/

import Std
open Std

private def format5 (x : Float) : String :=
  let y := x + 0.000005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00000").take 5
    else
      "00000"
  intPart ++ "." ++ fracPart

private def gauss (a : Array (Array Float)) (b : Array Float) : Array Float :=
  let n := a.size
  Id.run <| do
    let mut a := a
    let mut b := b
    for i in [0:n] do
      -- pivot selection
      let mut pivot := i
      for j in [i:n] do
        if Float.abs (a[j]![i]) > Float.abs (a[pivot]![i]) then
          pivot := j
      if Float.abs (a[pivot]![i]) > 1e-9 then
        -- swap rows
        if pivot ≠ i then
          let rowi := a[i]!
          a := a.set! i a[pivot]!
          a := a.set! pivot rowi
          let bi := b[i]!
          b := b.set! i b[pivot]!
          b := b.set! pivot bi
        let piv := a[i]![i]
        -- normalize pivot row
        let mut row := a[i]!
        for j in [i:n] do
          row := row.set! j (row[j]! / piv)
        a := a.set! i row
        b := b.set! i (b[i]! / piv)
        -- eliminate other rows
        for k in [0:n] do
          if k ≠ i then
            let factor := a[k]![i]
            if factor ≠ 0.0 then
              let mut rowk := a[k]!
              for j in [i:n] do
                rowk := rowk.set! j (rowk[j]! - factor * a[i]![j])
              a := a.set! k rowk
              b := b.set! k (b[k]! - factor * b[i]!)
    return b

private def solveCase (n : Nat) (P : Float) (L : String)
    (curr branch : Array String) : String :=
  let labelToIdx := Id.run <| do
    let mut m : Std.HashMap String Nat := {}
    for i in [0:n] do
      m := m.insert (curr.get! i) i
    return m
  let bIdx := Id.run <| do
    let mut arr : Array Nat := Array.mkEmpty n
    for i in [0:n] do
      arr := arr.push (labelToIdx.find! (branch.get! i))
    return arr
  let mut mat : Array (Array Float) := Array.mkArray n (Array.mkArray n 0.0)
  let mut vec : Array Float := Array.mkArray n 0.0
  for i in [0:n] do
    let lab := curr.get! i
    let delta := if lab = L then 1.0 else 0.0
    let mut row := mat[i]!
    row := row.set! i 1.0
    if lab = "start" then
      if h : i + 1 < n then
        row := row.set! (i+1) (-1.0)
      mat := mat.set! i row
      vec := vec.set! i delta
    else if lab = "end" then
      mat := mat.set! i row
      vec := vec.set! i delta
    else
      let j := bIdx.get! i
      row := row.set! j (row[j]! - P)
      if h : i + 1 < n then
        row := row.set! (i+1) (row[i+1]! - (1.0 - P))
      mat := mat.set! i row
      vec := vec.set! i delta
  let sol := gauss mat vec
  let startIdx := labelToIdx.find! "start"
  let ans := sol.get! startIdx
  s!"Expected number of times label {L} is executed is {format5 ans}"

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) :
    List String :=
  if t = 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let p := toks.get! (idx+1) |>.toFloat!
    let L := toks.get! (idx+2)
    let mut curr : Array String := Array.mkEmpty n
    let mut branch : Array String := Array.mkEmpty n
    let mut j := idx + 3
    for _ in [0:n] do
      curr := curr.push (toks.get! j)
      branch := branch.push (toks.get! (j+1))
      j := j + 2
    let out := solveCase n p L curr branch
    solveAll toks j (t-1) (out :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (fun s => s ≠ "")
                  |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 t []
  for line in outs do
    IO.println line
