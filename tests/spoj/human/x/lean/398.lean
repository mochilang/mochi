/- Solution for SPOJ RPGAMES - Roll Playing Games
https://www.spoj.com/problems/RPGAMES/
-/
import Std
open Std

-- compute number of ways to reach each sum using known dice
def computeWays (dice : Array (Array Nat)) : Array Nat :=
  let maxSum := dice.size * 50
  Id.run do
    let mut dp : Array Nat := Array.mkArray (maxSum+1) 0
    dp := dp.set! 0 1
    for die in dice do
      let mut ndp : Array Nat := Array.mkArray (maxSum+1) 0
      for s in [0:maxSum+1] do
        let ways := dp.get! s
        if ways != 0 then
          for v in die do
            let s2 := s + v
            if s2 <= maxSum then
              ndp := ndp.set! s2 (ndp.get! s2 + ways)
      dp := ndp
    return dp

-- subtract arrays element-wise
def subtract (a b : Array Nat) : Array Nat :=
  Id.run do
    let m := a.size
    let mut res := Array.mkArray m 0
    for j in [0:m] do
      res := res.set! j (a.get! j - b.get! j)
    return res

-- check all zeros
def allZero (arr : Array Nat) : Bool :=
  Id.run do
    let mut ok := true
    for j in [0:arr.size] do
      if arr.get! j != 0 then ok := false
    return ok

partial def dfs (r : Nat) (contrib : Array (Array Nat)) (m : Nat)
    (pos start : Nat) (rem : Array Nat) : Option (List Nat) :=
  if pos == r then
    if allZero rem then some [] else none
  else
    let rec loop (v : Nat) : Option (List Nat) :=
      if v > 50 then none else
        let cv := contrib.get! v
        let mut ok := true
        for j in [0:m] do
          if cv.get! j > rem.get! j then ok := false
        if ok then
          let rem2 := subtract rem cv
          match dfs r contrib m (pos+1) v rem2 with
          | some tail => some (v :: tail)
          | none => loop (v+1)
        else
          loop (v+1)
    loop start

partial def solveCase (dice : Array (Array Nat)) (r : Nat) (m : Nat)
    (vals cnts : Array Nat) : String :=
  let ways := computeWays dice
  let maxSum := dice.size * 50
  -- precompute contributions for each possible face value 1..50
  let contrib := Id.run do
    let mut arr : Array (Array Nat) := Array.mkArray 51 (Array.mkArray m 0)
    for v in [1:51] do
      let mut vec := Array.mkArray m 0
      for j in [0:m] do
        let target := vals.get! j
        if v <= target && target - v <= maxSum then
          vec := vec.set! j (ways.get! (target - v))
      arr := arr.set! v vec
    return arr
  match dfs r contrib m 0 1 cnts with
  | some faces =>
      let rec join (xs : List Nat) : String :=
        match xs with
        | [] => ""
        | [x] => toString x
        | x :: xs => toString x ++ " " ++ join xs
      "Final die face values are " ++ join faces
  | none => "Impossible"

partial def solve (toks : Array String) (i : Nat) (acc : List String) : List String :=
  let n := toks.get! i |>.toNat!
  if n == 0 then acc.reverse else
    let mut idx := i + 1
    let mut dice : Array (Array Nat) := Array.mkEmpty n
    for _ in [0:n] do
      let f := toks.get! idx |>.toNat!
      idx := idx + 1
      let mut die : Array Nat := Array.mkEmpty f
      for _ in [0:f] do
        die := die.push (toks.get! idx |>.toNat!)
        idx := idx + 1
      dice := dice.push die
    let r := toks.get! idx |>.toNat!
    let m := toks.get! (idx+1) |>.toNat!
    idx := idx + 2
    let mut vals : Array Nat := Array.mkEmpty m
    let mut cnts : Array Nat := Array.mkEmpty m
    for _ in [0:m] do
      vals := vals.push (toks.get! idx |>.toNat!)
      cnts := cnts.push (toks.get! (idx+1) |>.toNat!)
      idx := idx + 2
    let ans := solveCase dice r m vals cnts
    solve toks idx (ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "") |>.toArray
  let res := solve toks 0 []
  for s in res do
    IO.println s
