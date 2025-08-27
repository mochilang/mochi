/- Solution for SPOJ SQDANCE - Square dance
https://www.spoj.com/problems/SQDANCE/
-/

import Std
open Std

def solveCase (r c : Nat) (vals : Array Int) : Int :=
  Id.run do
    let n := r * c
    let mut up : Array (Option Nat) := Array.mkArray n none
    let mut down : Array (Option Nat) := Array.mkArray n none
    let mut left : Array (Option Nat) := Array.mkArray n none
    let mut right : Array (Option Nat) := Array.mkArray n none
    for i in [0:r] do
      for j in [0:c] do
        let idx := i * c + j
        if i > 0 then
          up := up.set! idx (some (idx - c))
        if i + 1 < r then
          down := down.set! idx (some (idx + c))
        if j > 0 then
          left := left.set! idx (some (idx - 1))
        if j + 1 < c then
          right := right.set! idx (some (idx + 1))
    let mut alive : Array Bool := Array.mkArray n true
    partial def loop (cands : Array Nat) (ans : Int) : Int :=
      let mut roundSum : Int := 0
      let mut toRemove : Array Nat := #[]
      for i in cands do
        if alive.get! i then
          roundSum := roundSum + vals.get! i
          let mut sum : Int := 0
          let mut cnt : Int := 0
          match up.get! i with
          | some u =>
            if alive.get! u then
              sum := sum + vals.get! u
              cnt := cnt + 1
          | none => ()
          match down.get! i with
          | some d =>
            if alive.get! d then
              sum := sum + vals.get! d
              cnt := cnt + 1
          | none => ()
          match left.get! i with
          | some l =>
            if alive.get! l then
              sum := sum + vals.get! l
              cnt := cnt + 1
          | none => ()
          match right.get! i with
          | some r =>
            if alive.get! r then
              sum := sum + vals.get! r
              cnt := cnt + 1
          | none => ()
          if cnt > 0 ∧ vals.get! i * cnt < sum then
            toRemove := toRemove.push i
      let ans := ans + roundSum
      if toRemove.isEmpty then
        ans
      else
        let mut next : Std.HashSet Nat := {}
        for i in toRemove do
          alive := alive.set! i false
          let u := up.get! i
          let d := down.get! i
          let l := left.get! i
          let r := right.get! i
          match u with
          | some x =>
            down := down.set! x d
            if alive.get! x then next := next.insert x
          | none => ()
          match d with
          | some x =>
            up := up.set! x u
            if alive.get! x then next := next.insert x
          | none => ()
          match l with
          | some x =>
            right := right.set! x r
            if alive.get! x then next := next.insert x
          | none => ()
          match r with
          | some x =>
            left := left.set! x l
            if alive.get! x then next := next.insert x
          | none => ()
        loop (Array.ofList next.toList) ans
    loop (Array.ofList (List.range n)) 0

private def readNatInts (line : String) : Array Int :=
  line.split (fun c => c = ' ') |>.filter (fun t => t ≠ "") |>.map (fun t => t.toInt!) |>.toArray

private def readNatPair (line : String) : Nat × Nat :=
  let ws := line.split (· = ' ') |>.filter (· ≠ "") |>.map String.toNat!
  (ws.get! 0, ws.get! 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let rcLine ← h.getLine
    let (r, c) := readNatPair rcLine
    let mut vals : Array Int := Array.mkArray (r * c) 0
    for i in [0:r] do
      let nums := readNatInts (← h.getLine)
      for j in [0:c] do
        vals := vals.set! (i * c + j) (nums.get! j)
    let ans := solveCase r c vals
    IO.println ans
