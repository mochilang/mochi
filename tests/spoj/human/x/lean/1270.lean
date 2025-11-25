import Std

open Std

-- https://www.spoj.com/problems/PNTBYNUM/

structure ColState where
  idx : Nat
  len : Nat
  deriving Inhabited

partial def genLine (m : Nat) (seq : List Nat) : List Nat :=
  let sumList (l : List Nat) := l.foldl (· + ·) 0
  let rec build (pos : Nat) (seq : List Nat) : List Nat :=
    match seq with
    | [] => [0]
    | k :: rest =>
        let after := if rest = [] then 0 else sumList rest + rest.length
        let rec loop (start : Nat) : List Nat :=
          if start + k + after > m then [] else
            let blockMask := Nat.shiftLeft ((Nat.pow 2 k) - 1) start
            let nextPos := start + k + (if rest = [] then 0 else 1)
            let tails := build nextPos rest
            let withBlock := tails.map (fun x => x ||| blockMask)
            withBlock ++ loop (start + 1)
        loop pos
  build 0 seq

partial def solve (n m : Nat) (rowSpecs : Array (List Nat)) (colSpecs : Array (List Nat)) : Array Nat :=
  let rowCands := rowSpecs.map (genLine m)
  let rec dfs (r : Nat) (cols : Array ColState) (acc : Array Nat) : Option (Array Nat) :=
    if h : r = n then
      let rec check (j : Nat) : Bool :=
        if hj : j < m then
          let st := cols.get ⟨j, hj⟩
          let spec := colSpecs.get ⟨j, by exact hj⟩
          if st.len > 0 then
            match spec.get? st.idx with
            | some target =>
                st.len == target && st.idx + 1 == spec.length && check (j+1)
            | none => false
          else
            st.idx == spec.length && check (j+1)
        else true
      if check 0 then some acc else none
    else
      let rec tryMasks (ms : List Nat) : Option (Array Nat) :=
        match ms with
        | [] => none
        | mask :: rest =>
            let rec upd (j : Nat) (cols : Array ColState) : Option (Array ColState) :=
              if hj : j < m then
                let st := cols.get ⟨j, hj⟩
                let spec := colSpecs.get ⟨j, by exact hj⟩
                let bit := Nat.testBit mask j
                if bit then
                  match spec.get? st.idx with
                  | some target =>
                      let newlen := st.len + 1
                      if newlen > target then none
                      else upd (j+1) (cols.set ⟨j, hj⟩ {st with len := newlen})
                  | none => none
                else
                  if st.len > 0 then
                    match spec.get? st.idx with
                    | some target =>
                        if st.len == target then
                          upd (j+1) (cols.set ⟨j, hj⟩ {idx := st.idx + 1, len := 0})
                        else none
                    | none => none
                  else
                    upd (j+1) cols
              else some cols
            match upd 0 cols with
            | some newCols =>
                match dfs (r+1) newCols (acc.push mask) with
                | some res => some res
                | none => tryMasks rest
            | none => tryMasks rest
      tryMasks (rowCands.get! r)
  match dfs 0 (Array.mkArray m ⟨0,0⟩) #[] with
  | some res => res
  | none => Array.mkArray n 0

def rowToString (m : Nat) (mask : Nat) : String :=
  String.mk <| (List.range m).map (fun j => if Nat.testBit mask j then '*' else '.')

partial def readNat : IO Nat := do
  return (← IO.getLine).trim.toNat!

partial def readSeq : IO (List Nat) := do
  let parts := (← IO.getLine).trim.splitOn " "
  let mut res : List Nat := []
  for p in parts do
    if p == "" then pure () else
    let v := p.toNat!
    if v == 0 then return res.reverse else res := v :: res
  return res.reverse

partial def solveCase : IO (List String) := do
  let n ← readNat
  let m ← readNat
  let mut rows : Array (List Nat) := Array.mkArray n []
  for i in [0:n] do
    rows := rows.set! i (← readSeq)
  let mut cols : Array (List Nat) := Array.mkArray m []
  for j in [0:m] do
    cols := cols.set! j (← readSeq)
  let solution := solve n m rows cols
  let mut out : List String := []
  for i in [0:n] do
    out := rowToString m (solution.get! i) :: out
  return out.reverse

partial def main : IO Unit := do
  let t ← readNat
  let mut first := true
  for _ in [0:t] do
    let grid ← solveCase
    if !first then IO.println ""
    first := false
    for row in grid do
      IO.println row
