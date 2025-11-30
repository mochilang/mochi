/- Solution for SPOJ SETSTACK - The SetStack Computer
https://www.spoj.com/problems/SETSTACK/
-/

import Std
open Std

-- merge two sorted lists without duplicates (union)
def ordUnion : List Nat → List Nat → List Nat
  | [], l => l
  | l, [] => l
  | x::xs, y::ys =>
      if x == y then x :: ordUnion xs ys
      else if x < y then x :: ordUnion xs (y::ys)
      else y :: ordUnion (x::xs) ys

-- intersection of two sorted lists
def ordInter : List Nat → List Nat → List Nat
  | [], _ => []
  | _, [] => []
  | x::xs, y::ys =>
      if x == y then x :: ordInter xs ys
      else if x < y then ordInter xs (y::ys)
      else ordInter (x::xs) ys

-- insert an element into a sorted list if not present
def ordInsert (a : Nat) : List Nat → List Nat
  | [] => [a]
  | x::xs =>
      if a == x then x::xs
      else if a < x then a :: x :: xs
      else x :: ordInsert a xs

-- obtain or assign an ID for a set
def getId (s : List Nat)
  (idMap : Std.HashMap (List Nat) Nat)
  (sets : Array (List Nat))
  : Std.HashMap (List Nat) Nat × Array (List Nat) × Nat :=
  match idMap.find? s with
  | some id => (idMap, sets, id)
  | none =>
      let id := sets.size
      (idMap.insert s id, sets.push s, id)

def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks[0]!.toNat!
    let mut idx := 1
    let mut outs : Array String := #[]
    for _ in [0:t] do
      let n := toks[idx]!.toNat!
      idx := idx + 1
      let mut idMap : Std.HashMap (List Nat) Nat := {}
      let mut sets : Array (List Nat) := #[]
      let mut stack : Array Nat := #[]
      for _ in [0:n] do
        let cmd := toks[idx]!
        idx := idx + 1
        match cmd with
        | "PUSH" =>
            let (m, sArr, id) := getId [] idMap sets
            idMap := m
            sets := sArr
            stack := stack.push id
            outs := outs.push (toString (sets[id]!.length))
        | "DUP" =>
            let top := stack.back!
            stack := stack.push top
            outs := outs.push (toString (sets[top]!.length))
        | "UNION" =>
            let a := stack.back!
            stack := stack.pop
            let b := stack.back!
            stack := stack.pop
            let s := ordUnion sets[a]! sets[b]!
            let (m, sArr, id) := getId s idMap sets
            idMap := m
            sets := sArr
            stack := stack.push id
            outs := outs.push (toString (sets[id]!.length))
        | "INTERSECT" =>
            let a := stack.back!
            stack := stack.pop
            let b := stack.back!
            stack := stack.pop
            let s := ordInter sets[a]! sets[b]!
            let (m, sArr, id) := getId s idMap sets
            idMap := m
            sets := sArr
            stack := stack.push id
            outs := outs.push (toString (sets[id]!.length))
        | "ADD" =>
            let a := stack.back!
            stack := stack.pop
            let b := stack.back!
            stack := stack.pop
            let s := ordInsert a sets[b]!
            let (m, sArr, id) := getId s idMap sets
            idMap := m
            sets := sArr
            stack := stack.push id
            outs := outs.push (toString (sets[id]!.length))
        | _ => pure ()
      outs := outs.push "***"
    return outs

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let res := solve toks
  for line in res do
    IO.println line
