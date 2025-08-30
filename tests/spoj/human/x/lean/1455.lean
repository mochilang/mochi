/- Solution for SPOJ ANALYS - Program Analysis
https://www.spoj.com/problems/ANALYS/
-/

import Std
open Std

inductive Instr
| add (v : Nat) (n : Int)
| output (v : Nat)
| goto (lvl : Nat)
| condGoto (v : Nat) (n : Int) (lvl : Nat)
| stop

structure Stmt where
  level : Nat
  instr : Instr

/-- Map variable character to index 0..25. -/
def varIdx (c : Char) : Nat := c.toNat - 'A'.toNat

/-- Parse a single line into a statement. -/
def parseLine (line : String) : Stmt :=
  let line := line.trim
  let lvlStr := line.takeWhile Char.isDigit
  let rest := (line.dropWhile Char.isDigit).dropWhile (· = ' ')
  let lvl := lvlStr.toNat!
  if rest == "END" then { level := lvl, instr := Instr.stop }
  else if rest.startsWith "GO " then
    { level := lvl, instr := Instr.goto ((rest.drop 3).toNat!) }
  else if rest.startsWith "IF " then
    let after := rest.drop 3
    let parts := after.splitOn " GO "
    let cond := parts.get! 0
    let target := (parts.get! 1).toNat!
    let cp := cond.splitOn "="
    let v := varIdx (cp.get! 0).data.head!
    let n := (cp.get! 1).toNat!
    { level := lvl, instr := Instr.condGoto v n target }
  else if (rest.data.get! 1) = '?' then
    { level := lvl, instr := Instr.output (varIdx rest.data.head!) }
  else
    let v := varIdx rest.data.head!
    let n := (rest.drop 2).toNat!
    { level := lvl, instr := Instr.add v n }

/-- Sort statements by level using insertion sort. -/
def sortStmts : List Stmt → List Stmt
| [] => []
| s :: ss =>
  let rec insert (x : Stmt) : List Stmt → List Stmt
  | [] => [x]
  | y :: ys => if x.level < y.level then x :: y :: ys else y :: insert x ys
  insert s (sortStmts ss)

/-- Find index of a level in the array. -/
def findIndex (a : Array Stmt) (lvl : Nat) : Nat :=
  let rec loop (i : Nat) : Nat :=
    if h : i < a.size then
      let s := a.get ⟨i, h⟩
      if s.level = lvl then i else loop (i+1)
    else 0
  loop 0

/-- Execute program and count executed statements. Returns -1 on non-termination. -/
def run (prog : Array Stmt) : Int :=
  let limit : Nat := 1000000
  let rec step (pc : Nat) (vars : Array Int) (cnt : Nat) : Int :=
    if cnt > limit then -1
    else
      let st := prog.get! pc
      let cnt := cnt + 1
      match st.instr with
      | Instr.add v n =>
        let vars := vars.set! v (vars.get! v + n)
        step (pc+1) vars cnt
      | Instr.output _ => step (pc+1) vars cnt
      | Instr.goto lvl => step (findIndex prog lvl) vars cnt
      | Instr.condGoto v n lvl =>
        let pc' := if vars.get! v = n then findIndex prog lvl else pc+1
        step pc' vars cnt
      | Instr.stop => (cnt : Int)
  step 0 (Array.mkArray 26 0) 0

/-- Read program, execute and print number of executed sentences. -/
def main : IO Unit := do
  let data ← IO.getStdin.readToEnd
  let lines := data.toString.splitOn "\n"
  let stmts := lines.foldl (fun acc l => if l.trim = "" then acc else parseLine l :: acc) []
  let sorted := sortStmts stmts
  let prog := sorted.toArray
  IO.println s!"{run prog}"
