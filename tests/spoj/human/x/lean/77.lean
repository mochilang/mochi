/- Solution for SPOJ BRICKS - New bricks disorder
https://www.spoj.com/problems/BRICKS/
-/

import Std
open Std

def charIdx (c : Char) : Nat := c.toNat - 'a'.toNat

def countGT (xs : List Nat) (p : Nat) : Nat :=
  xs.foldl (fun acc x => if x > p then acc + 1 else acc) 0

partial def solveCase (s t : String) (pairs : List (Char × Char)) : String := Id.run do
  if s.length ≠ t.length then
    return "-1"
  else
    -- build target positions lists
    let posInit : Array (List Nat) := Array.replicate 26 []
    let rec build (i : Nat) (cs : List Char) (acc : Array (List Nat)) : Array (List Nat) :=
      match cs with
      | [] => acc
      | c :: cs =>
        let idx := charIdx c
        let acc := acc.modify idx (fun l => i :: l)
        build (i+1) cs acc
    let posLists := (build 0 t.data posInit).map (fun l => l.reverse)
    -- allowed matrix
    let mut allow : Array (Array Bool) := Array.replicate 26 (Array.replicate 26 false)
    for (a,b) in pairs do
      let ia := charIdx a
      let ib := charIdx b
      let rowA := allow[ia]!
      let rowA := rowA.set! ib true
      allow := allow.set! ia rowA
      let rowB := allow[ib]!
      let rowB := rowB.set! ia true
      allow := allow.set! ib rowB
    -- processed lists for each letter
    let mut proc : Array (List Nat) := Array.replicate 26 []
    let mut cross : Array (Array Nat) := Array.replicate 26 (Array.replicate 26 0)
    let mut total : Nat := 0
    let rec loop (cs : List Char) (posLists : Array (List Nat)) (proc : Array (List Nat)) (cross : Array (Array Nat)) (total : Nat) (possible : Bool) : Id String := do
      match cs with
      | [] =>
        if possible then
          let ok := Id.run do
            let mut flag := true
            for a in [0:26] do
              let row := cross[a]!
              for b in [0:26] do
                if row[b]! > 0 ∧ ¬allow[a]![b]! then
                  flag := false
            return flag
          if ok then
            let modv := total % (1 <<< 32)
            return toString modv
          else
            return "-1"
        else
          return "-1"
      | c :: cs =>
        if possible then
          let idx := charIdx c
          let lst := posLists[idx]!
          match lst with
          | [] =>
            loop cs posLists proc cross total false
          | p :: rest =>
            let mut cross1 := cross
            let mut total1 := total
            for d in [0:26] do
              let g := countGT (proc[d]!) p
              if g > 0 then
                let row := cross1[d]!
                let row := row.set! idx (row[idx]! + g)
                cross1 := cross1.set! d row
                total1 := total1 + g
            let proc' := proc.set! idx (p :: proc[idx]!)
            let posLists' := posLists.set! idx rest
            loop cs posLists' proc' cross1 total1 true
        else
          loop cs posLists proc cross total false
    let res ← loop s.data posLists proc cross total true
    return res

partial def parseAll (toks : Array String) (idx c : Nat) (acc : List String) : List String :=
  if c == 0 then acc.reverse else
    let s := toks[idx]!
    let t := toks[idx+1]!
    let m := (toks[idx+2]!).toNat!
    let pairs := Id.run do
      let mut ps : List (Char × Char) := []
      for j in [0:m] do
        let pr := toks[idx+3+j]!
        let arr := pr.data.toArray
        let a := arr[0]!
        let b := arr[1]!
        ps := (a,b) :: ps
      return ps
    let res := solveCase s t pairs
    parseAll toks (idx+3+m) (c-1) (res :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let c := (toks[0]!).toNat!
    let outputs := parseAll toks 1 c []
    IO.println (String.intercalate "\n" outputs)
