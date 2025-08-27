/- Solution for SPOJ ROADNET - Road net
https://www.spoj.com/problems/ROADNET/
-/
import Std
open Std

partial def readMatrix (nums : Array Nat) (idx : Nat) (n i j : Nat)
    (mat : Array (Array Nat)) : (Array (Array Nat) × Nat) :=
  if h : i < n then
    if h2 : j < n then
      let val := nums.get! idx
      let row := (mat.get! i).set! j val
      let mat := mat.set! i row
      readMatrix nums (idx+1) n i (j+1) mat
    else
      readMatrix nums idx n (i+1) 0 mat
  else
    (mat, idx)

partial def isNeighbor (mat : Array (Array Nat)) (n i j k : Nat) : Bool :=
  if h : k < n then
    if k = i || k = j then
      isNeighbor mat n i j (k+1)
    else
      let dij := (mat.get! i).get! j
      let sum := (mat.get! i).get! k + (mat.get! k).get! j
      if sum = dij then
        false
      else
        isNeighbor mat n i j (k+1)
  else
    true

partial def findPairs (mat : Array (Array Nat)) (n i j : Nat)
    (acc : Array String) : Array String :=
  if hi : i < n then
    if hj : j < n then
      let acc :=
        if i < j && isNeighbor mat n i j 0 then
          acc.push s!"{i+1} {j+1}"
        else
          acc
      findPairs mat n i (j+1) acc
    else
      findPairs mat n (i+1) (i+2) acc
  else
    acc

def solveCase (mat : Array (Array Nat)) (n : Nat) : Array String :=
  findPairs mat n 0 1 #[]

partial def parseCases (nums : Array Nat) (idx t : Nat) (acc : Array String)
    : Array String :=
  if t = 0 then
    acc
  else
    let n := nums.get! idx
    let (mat, nextIdx) :=
      readMatrix nums (idx+1) n 0 0 (Array.mkArray n (Array.mkArray n 0))
    let acc := (solveCase mat n).foldl (fun a line => a.push line) acc
    let acc := if t > 1 then acc.push "" else acc
    parseCases nums nextIdx (t-1) acc

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let nums := toks.map (fun s => s.toNat!)
  let t := nums.get! 0
  let res := parseCases nums 1 t #[]
  for line in res do
    IO.println line
