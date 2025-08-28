/- Solution for SPOJ LOGIC - Logic
https://www.spoj.com/problems/LOGIC/
-/

import Std
open Std

structure Node where
  op : Char
  inputs : List (Nat × Nat)
  deriving Inhabited

partial def eval (coord : Nat × Nat)
    (nodes : Std.HashMap (Nat × Nat) Node)
    (inVals : Std.HashMap (Nat × Nat) Bool)
    (memo : IO.Ref (Std.HashMap (Nat × Nat) Bool)) : IO Bool := do
  match (← memo.get).find? coord with
  | some v => return v
  | none =>
      let node := nodes.find! coord
      let v ←
        match node.op with
        | 'i' => pure (inVals.find! coord)
        | 'o' =>
            match node.inputs with
            | a :: _ => eval a nodes inVals memo
            | _ => pure false
        | '!' =>
            match node.inputs with
            | a :: _ => do let b ← eval a nodes inVals memo; return !b
            | _ => pure false
        | '&' =>
            match node.inputs with
            | a :: b :: _ =>
                let va ← eval a nodes inVals memo
                let vb ← eval b nodes inVals memo
                return va && vb
            | _ => pure false
        | '|' =>
            match node.inputs with
            | a :: b :: _ =>
                let va ← eval a nodes inVals memo
                let vb ← eval b nodes inVals memo
                return va || vb
            | _ => pure false
        | _ => pure false
      memo.modify (·.insert coord v)
      return v

def parseCoord (s : String) : Nat × Nat :=
  let c := s.data
  ((c.get! 0).toNat - '0'.toNat, (c.get! 1).toNat - '0'.toNat)

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let toks := input.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r').filter (· ≠ "")
  let arr : Array String := toks.toArray
  let mut idx : Nat := 0
  let next := fun () =>
    let t := arr.get! idx
    idx := idx + 1
    t
  let circuits := (next ()).toNat!
  for _ in [0:circuits] do
    let mut nodes : Std.HashMap (Nat × Nat) Node := {}
    let mut inOrder : Array (Nat × Nat) := #[]
    let mut outOrder : Array (Nat × Nat) := #[]
    while true do
      let tok := next()
      if tok = "end" then
        break
      let c := tok.data
      let x := (c.get! 0).toNat - '0'.toNat
      let y := (c.get! 1).toNat - '0'.toNat
      let op := c.get! 2
      let node :=
        match nodes.find? (x, y) with
        | some n => {n with op := op}
        | none => {op := op, inputs := []}
      nodes := nodes.insert (x, y) node
      if op = 'i' then
        inOrder := inOrder.push (x, y)
      if op = 'o' then
        outOrder := outOrder.push (x, y)
      while true do
        let s := next()
        if s = ".." then
          break
        let (tx, ty) := parseCoord s
        let tgt :=
          match nodes.find? (tx, ty) with
          | some n => {n with inputs := (x, y) :: n.inputs}
          | none => {op := '#', inputs := [(x, y)]}
        nodes := nodes.insert (tx, ty) tgt
    let tCases := (next ()).toNat!
    for _ in [0:tCases] do
      let line := next()
      let mut inVals : Std.HashMap (Nat × Nat) Bool := {}
      for i in [0:inOrder.size] do
        let coord := inOrder.get! i
        let b := (line.data.get! i) = '1'
        inVals := inVals.insert coord b
      let memo ← IO.mkRef ({} : Std.HashMap (Nat × Nat) Bool)
      let mut outStr := ""
      for coord in outOrder do
        let v ← eval coord nodes inVals memo
        outStr := outStr.push (if v then '1' else '0')
      IO.println outStr
    IO.println ""
