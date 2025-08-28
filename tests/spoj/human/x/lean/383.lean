/- Solution for SPOJ EUROPEAN - European railroad tracks
https://www.spoj.com/problems/EUROPEAN/
-/

import Std
open Std

/-- check if gauge g is present as difference of some pair of rails --/
def hasDiff (rails : List Nat) (g : Nat) : Bool :=
  let rec aux : List Nat → Bool
    | [] => False
    | x :: xs =>
        if xs.any (fun y => y - x == g) then true else aux xs
  aux rails

/-- rails positions that do not yet satisfy some gauges --/
def unsatisfied (rails gauges : List Nat) : List Nat :=
  gauges.filter (fun g => ¬ hasDiff rails g)

/-- insert x into sorted list of rails, ignoring duplicates --/
def insertSorted (x : Nat) : List Nat → List Nat
  | [] => [x]
  | y :: ys =>
      if x < y then x :: y :: ys
      else if x == y then y :: ys
      else y :: insertSorted x ys

/-- remove duplicates while preserving order --/
def dedup (xs : List Nat) : List Nat :=
  let rec go (seen : List Nat) : List Nat → List Nat
    | [] => seen.reverse
    | y :: ys => if seen.contains y then go seen ys else go (y :: seen) ys
  go [] xs

/-- backtracking search for minimal rails --/
partial def search (rails : List Nat) (gauges : List Nat) (maxRails limit : Nat) : Option (List Nat) :=
  let unsat := unsatisfied rails gauges
  if unsat.isEmpty then some rails
  else if rails.length ≥ maxRails then none
  else
    let rec tryG : List Nat → Option (List Nat)
      | [] => none
      | g :: gs =>
          let cands := dedup (rails.map (fun r => r + g))
                        |>.filter (fun x => x ≤ limit ∧ ¬ rails.contains x)
          let rec tryPos : List Nat → Option (List Nat)
            | [] => tryG gs
            | p :: ps =>
                match search (insertSorted p rails) gauges maxRails limit with
                | some res => some res
                | none => tryPos ps
          tryPos cands
    tryG unsat

/-- helper to search over rail counts --/
partial def findSolution (gs : List Nat) (limit : Nat) (k : Nat) : List Nat :=
  if k > 5 then [0]
  else
    match search [0] gs k limit with
    | some rails => rails
    | none => findSolution gs limit (k+1)

/-- solve one test case --/
def solveCase (gauges : List Nat) : List Nat :=
  let gs := dedup gauges
  let maxG := gs.foldl Nat.max 0
  let limit := maxG * 2
  findSolution gs limit 1

partial def readList (toks : Array String) (idx n : Nat) (acc : List Nat) : (List Nat × Nat) :=
  if n = 0 then (acc.reverse, idx)
  else
    let x := (toks[idx]!).toNat!
    readList toks (idx+1) (n-1) (x :: acc)

partial def solveAll (toks : Array String) (idx case t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let n := (toks[idx]!).toNat!
    let (gs, idx') := readList toks (idx+1) n []
    let rails := solveCase gs
    let line1 := s!"Scenario #{case}"
    let positions := rails.map (fun x => toString x)
    let line2 := s!"{rails.length}: " ++ String.intercalate " " positions
    solveAll toks idx' (case+1) (t-1) ("" :: line2 :: line1 :: acc)

/-- read entire stdin --/
def readStdin : IO String := do
  let h ← IO.getStdin
  h.readToEnd

def main : IO Unit := do
  let data ← readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let t := (toks[0]!).toNat!
  let outs := solveAll toks 1 1 t []
  for line in outs do
    IO.println line
