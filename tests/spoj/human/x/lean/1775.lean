/- Solution for SPOJ BOOLE - Boolean Logic
https://www.spoj.com/problems/BOOLE/
-/

import Std
open Std

inductive BinOp | And | Or | Imp | Equiv deriving Repr

inductive Expr where
| var (c : Char) (pos : Nat) : Expr
| not (pos : Nat) (e : Expr) : Expr
| bin (op : BinOp) (pos : Nat) (a b : Expr) : Expr
deriving Inhabited

partial def skipSpaces (s : Array Char) : Nat → Nat
  | i =>
    if _h : i < s.size then
      if s[i]! = ' ' then skipSpaces s (i+1) else i
    else i

partial def parseOp (s : Array Char) (i : Nat) : (BinOp × Nat × Nat) :=
  match s[i]! with
  | '&' => (BinOp.And, i, i+1)
  | '|' => (BinOp.Or, i, i+1)
  | '-' => (BinOp.Imp, i+1, i+3)
  | '<' => (BinOp.Equiv, i+1, i+3)
  | _   => (BinOp.And, i, i+1) -- unreachable

partial def parseExpr (s : Array Char) : Nat → (Expr × Nat)
  | i =>
    let i := skipSpaces s i
    if _h : i < s.size then
      let c := s[i]!
      if c = '(' then
        let j := skipSpaces s (i+1)
        if s[j]! = '!' then
          let pos := j
          let (e1, k) := parseExpr s (j+1)
          let k := skipSpaces s k
          (Expr.not pos e1, k+1)
        else
          let (a, j1) := parseExpr s j
          let j1 := skipSpaces s j1
          let (op, pos, j2) := parseOp s j1
          let (b, j3) := parseExpr s j2
          let j3 := skipSpaces s j3
          (Expr.bin op pos a b, j3+1)
      else
        (Expr.var c i, i+1)
    else
      (Expr.var 'a' i, i) -- unreachable

partial def collectVars : Expr → List Char
| Expr.var c _ => [c]
| Expr.not _ e => collectVars e
| Expr.bin _ _ a b => collectVars a ++ collectVars b

partial def evalExpr : Expr → List (Char × Bool) → (Bool × List (Nat × Bool))
| Expr.var c pos, env =>
    let v := (env.find? (fun p => p.fst = c)).map (·.snd) |>.getD false
    (v, [(pos, v)])
| Expr.not pos e, env =>
    let (v1, m1) := evalExpr e env
    let v := !v1
    (v, m1 ++ [(pos, v)])
| Expr.bin op pos a b, env =>
    let (va, ma) := evalExpr a env
    let (vb, mb) := evalExpr b env
    let v := match op with
      | BinOp.And   => va && vb
      | BinOp.Or    => va || vb
      | BinOp.Imp   => (!va) || vb
      | BinOp.Equiv => va == vb
    (v, ma ++ mb ++ [(pos, v)])

def render (len : Nat) (vals : List (Nat × Bool)) : String :=
  Id.run do
    let mut arr : Array Char := Array.replicate len ' '
    for (p, v) in vals do
      arr := arr.set! p (if v then '1' else '0')
    return String.mk arr.toList

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  for line in data.splitOn "\n" do
    if line.length = 0 then
      continue
    IO.println line
    let chars := line.toList.toArray
    let (expr, _) := parseExpr chars 0
    let vars := (List.toArray (collectVars expr).eraseDups).qsort (· < ·)
    let n := vars.size
    let total := Nat.pow 2 n
    for mask in [0:total] do
      let env : List (Char × Bool) :=
        vars.toList.zipIdx.map (fun (c, i) => (c, Nat.testBit mask (n - 1 - i)))
      let (_, vals) := evalExpr expr env
      IO.println (render line.length vals)
    IO.println ""
