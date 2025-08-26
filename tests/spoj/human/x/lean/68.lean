/- Solution for SPOJ EXPR1 - Expression
https://www.spoj.com/problems/EXPR1/
-/

import Std
open Std

def updateMin (m : Std.HashMap Int Nat) (key : Int) (val : Nat) : Std.HashMap Int Nat :=
  match m.get? key with
  | some old => if val < old then m.insert key val else m
  | none => m.insert key val

partial def solveSeg (nums : Array Int) (ops : Array Char)
    (memoRef : IO.Ref (Std.HashMap (Nat × Nat) (Std.HashMap Int Nat)))
    (l r : Nat) : IO (Std.HashMap Int Nat) := do
  if h : l == r then
    let v := nums[l]!
    return Std.HashMap.empty.insert v 0
  else
    let memo ← memoRef.get
    match memo.get? (l, r) with
    | some res => return res
    | none => do
      let mut res : Std.HashMap Int Nat := Std.HashMap.empty
      for k in [l:r] do
        let left ← solveSeg nums ops memoRef l k
        let right ← solveSeg nums ops memoRef (k+1) r
        let op := ops[k]!
        let extra := if k == r - 1 then 0 else 1
        for (vL, cL) in left.toList do
          for (vR, cR) in right.toList do
            let v := if op == '+' then vL + vR else vL - vR
            let c := cL + cR + extra
            res := updateMin res v c
      memoRef.modify (fun m => m.insert (l, r) res)
      return res

def parseExpr (s : String) : (Array Int × Array Char) :=
  let arr := s.data.toArray
  let m := arr.size / 2
  let rec loop (i : Nat) (nums : Array Int) (ops : Array Char) :=
    if h : i < m then
      let op := arr[2*i]!
      let digit := arr[2*i + 1]!
      let num : Int := Int.ofNat (digit.toNat - '0'.toNat)
      loop (i+1) (nums.push num) (ops.push op)
    else
      (nums, ops)
  loop 0 #[0] #[]

partial def solve (expr : String) (target : Int) : IO (Option Nat) := do
  let (nums, ops) := parseExpr expr
  let memo ← IO.mkRef (Std.HashMap.empty : Std.HashMap (Nat × Nat) (Std.HashMap Int Nat))
  let res ← solveSeg nums ops memo 0 (nums.size - 1)
  return res.get? target

partial def main : IO Unit := do
  let h ← IO.getStdin
  let dLine ← h.getLine
  let d := dLine.trim.toNat!
  for _ in [0:d] do
    let line ← h.getLine
    let parts := line.trim.split (· == ' ')
    let k := (parts.get! 1).toInt!
    let expr ← h.getLine
    let expr := expr.trim
    let ans ← solve expr k
    match ans with
    | some v => IO.println s!"{v}"
    | none => IO.println "NO"
