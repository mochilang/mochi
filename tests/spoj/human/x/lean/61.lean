/- Solution for SPOJ BRCKTS - Brackets
https://www.spoj.com/problems/BRCKTS/
-/

import Std
open Std

structure Node where
  sum : Int
  minPref : Int

def combine (a b : Node) : Node :=
  let s := a.sum + b.sum
  let cand := a.sum + b.minPref
  let mn := if a.minPref < cand then a.minPref else cand
  { sum := s, minPref := mn }

def fromVal (v : Int) : Node :=
  { sum := v, minPref := if v < 0 then v else 0 }

partial def buildAux (arr : Array Int) (tree : Array Node) (idx l r : Nat) : Array Node :=
  if h : l = r then
    let v := arr.get! l
    tree.set! idx (fromVal v)
  else
    let mid := (l + r) / 2
    let tree := buildAux arr tree (2*idx + 1) l mid
    let tree := buildAux arr tree (2*idx + 2) (mid + 1) r
    let left := tree.get! (2*idx + 1)
    let right := tree.get! (2*idx + 2)
    tree.set! idx (combine left right)

def build (arr : Array Int) : Array Node :=
  if arr.size = 0 then Array.mkEmpty 0
  else buildAux arr (Array.mkArray (4 * arr.size) {sum := 0, minPref := 0}) 0 0 (arr.size - 1)

partial def updateAux (tree : Array Node) (idx l r pos : Nat) (value : Int) : Array Node :=
  if l = r then
    tree.set! idx (fromVal value)
  else
    let mid := (l + r) / 2
    let tree :=
      if pos ≤ mid then
        updateAux tree (2*idx + 1) l mid pos value
      else
        updateAux tree (2*idx + 2) (mid + 1) r pos value
    let left := tree.get! (2*idx + 1)
    let right := tree.get! (2*idx + 2)
    tree.set! idx (combine left right)

partial def processTest (h : IO.FS.Stream) (test : Nat) : IO Bool := do
  match (← h.getLine?) with
  | none => pure false
  | some line =>
      let line := line.trim
      if line.isEmpty then
        processTest h test
      else
        let n := line.toNat!
        let s := (← h.getLine).trim
        let m := (← h.getLine).trim.toNat!
        let mut arr : Array Int := Array.mkEmpty n
        for c in s.data do
          arr := arr.push (if c = '(' then (1 : Int) else -1)
        let mut tree := build arr
        IO.println s!"Test {test}:"
        for _ in List.range m do
          let k := (← h.getLine).trim.toNat!
          if k = 0 then
            let root := tree.get! 0
            if root.sum = 0 ∧ root.minPref ≥ 0 then
              IO.println "YES"
            else
              IO.println "NO"
          else
            let idx := k - 1
            let v := arr.get! idx
            let newVal := -v
            arr := arr.set! idx newVal
            tree := updateAux tree 0 0 (n - 1) idx newVal
        pure true

partial def loop (h : IO.FS.Stream) (test : Nat) : IO Unit := do
  let cont ← processTest h test
  if cont then loop h (test + 1) else pure ()

def main : IO Unit := do
  loop (← IO.getStdin) 1
