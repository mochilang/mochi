/- Solution for SPOJ TAUT - Tautology
https://www.spoj.com/problems/TAUT/
-/

import Std
open Std

inductive Expr
| var (c : Char)
| neg (e : Expr)
| conj (a b : Expr)
| disj (a b : Expr)
| impl (a b : Expr)
| iff (a b : Expr)

def parse : List Char -> Expr × List Char
  | [] => (.var 'p', [])
  | c :: rest =>
    match c with
    | 'C' => let (a, r1) := parse rest; let (b, r2) := parse r1; (.conj a b, r2)
    | 'D' => let (a, r1) := parse rest; let (b, r2) := parse r1; (.disj a b, r2)
    | 'I' => let (a, r1) := parse rest; let (b, r2) := parse r1; (.impl a b, r2)
    | 'E' => let (a, r1) := parse rest; let (b, r2) := parse r1; (.iff a b, r2)
    | 'N' => let (a, r1) := parse rest; (.neg a, r1)
    | v   => (.var v, rest)

def parseStr (s : String) : Expr := (parse s.data).fst

def evalExpr : Expr -> List (Char × Bool) -> Bool
  | .var c, env => (env.find? (fun p => p.fst = c)).map (fun p => p.snd) |>.getD false
  | .neg e, env => ! evalExpr e env
  | .conj a b, env => evalExpr a env && evalExpr b env
  | .disj a b, env => evalExpr a env || evalExpr b env
  | .impl a b, env => (! evalExpr a env) || evalExpr b env
  | .iff a b, env =>
      let ea := evalExpr a env
      let eb := evalExpr b env
      (ea && eb) || ((!ea) && (!eb))

def varsIn (s : String) : List Char :=
  s.foldl (fun acc c => if c.isLower && !acc.contains c then c :: acc else acc) []

def solve (line : String) : Bool :=
  let expr := parseStr line
  let vars := varsIn line
  let n := vars.length
  let total := Nat.pow 2 n
  let rec loop (mask : Nat) : Bool :=
    if mask = total then true
    else
      let env := vars.enum.map (fun ⟨i, c⟩ => (c, Nat.testBit mask i))
      if evalExpr expr env then loop (mask + 1) else false
  loop 0

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let ans := if solve line.trim then "YES" else "NO"
    IO.println ans
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
