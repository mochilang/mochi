/- Solution for SPOJ ASCIRC - Assembler circuits
https://www.spoj.com/problems/ASCIRC/
-/
import Std.Data.HashMap
import Std.Data.HashSet
open Std

structure Key where
  op : Char
  a  : Nat
  b  : Nat
  deriving BEq, Hashable

private def regIdx (c : Char) : Nat := c.toNat - 'a'.toNat

partial def solveCase (toks : Array String) (idx : Nat) : (String × Nat) :=
  Id.run do
    let n := toks[idx]!.toNat!
    let mut idx := idx + 1
    let mut regs : Array Nat := (List.range 26).toArray
    let mut childs : Array (Nat × Nat) := Array.replicate 26 (0,0)
    let mut mp : Std.HashMap Key Nat := {}
    let mut nextId : Nat := 26
    for _ in [0:n] do
      let w := toks[idx]!
      idx := idx + 1
      let chars := w.data.toArray
      let op := chars[0]!
      let r1 := chars[1]!
      let r2 := chars[2]!
      let rd := chars[3]!
      let id1 := regs[regIdx r1]!
      let id2 := regs[regIdx r2]!
      let key : Key := {op := op, a := id1, b := id2}
      let nodeId ←
        match mp.get? key with
        | some v => pure v
        | none => do
          mp := mp.insert key nextId
          childs := childs.push (id1, id2)
          let v := nextId
          nextId := nextId + 1
          pure v
      regs := regs.set! (regIdx rd) nodeId
    -- traverse from final registers to count needed gates
    let mut vis : Std.HashSet Nat := {}
    let mut stack : List Nat := regs.toList
    let mut cnt : Nat := 0
    while stack ≠ [] do
      let id := stack.head!
      stack := stack.tail!
      if id ≥ 26 ∧ ¬vis.contains id then
        vis := vis.insert id
        cnt := cnt + 1
        let (l,r) := childs[id]!
        stack := l :: r :: stack
    (toString cnt, idx)

partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let (res, idx) := solveCase toks idx
    process toks idx (t-1) (res :: acc)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |>.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    let outs := process toks 1 t []
    for line in outs do
      IO.println line
