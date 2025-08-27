/- Solution for SPOJ SOPARADE - Soldiers on Parade
https://www.spoj.com/problems/SOPARADE/
-/

import Std
open Std

structure TwoSAT where
  n : Nat
  adj : Array (List Nat)
  radj : Array (List Nat)

namespace TwoSAT

@[inline] def lit (i : Nat) (b : Bool) : Nat := 2*i + (if b then 1 else 0)
@[inline] def neg (x : Nat) : Nat := if x % 2 == 0 then x + 1 else x - 1

def mk (n : Nat) : TwoSAT :=
  { n := n, adj := Array.mkArray (2*n) [], radj := Array.mkArray (2*n) [] }

@[inline] def addImp (ts : TwoSAT) (u v : Nat) : TwoSAT :=
  { ts with
    adj := ts.adj.set! u (v :: ts.adj.get! u),
    radj := ts.radj.set! v (u :: ts.radj.get! v) }

@[inline] def addOr (ts : TwoSAT) (a b : Nat) : TwoSAT :=
  let ts := ts.addImp (neg a) b
  let ts := ts.addImp (neg b) a
  ts

@[inline] def addXor (ts : TwoSAT) (i j : Nat) : TwoSAT :=
  let ts := ts.addOr (lit i true) (lit j true)
  let ts := ts.addOr (lit i false) (lit j false)
  ts

partial def satisfiable (ts : TwoSAT) : Bool :=
  Id.run do
    let total := ts.adj.size
    let mut visited : Array Bool := Array.mkArray total false
    let mut order : Array Nat := Array.mkArray total 0
    let mut idx : Nat := 0
    for v in [:total] do
      if !(visited.get! v) then
        let mut stack : List (Nat × Bool) := [(v,false)]
        visited := visited.set! v true
        while stack ≠ [] do
          match stack with
          | (x,false) :: rest =>
            stack := (x,true) :: rest
            for u in ts.adj.get! x do
              if !(visited.get! u) then
                visited := visited.set! u true
                stack := (u,false) :: stack
          | (x,true) :: rest =>
            order := order.set! idx x
            idx := idx + 1
            stack := rest
          | [] => pure () -- unreachable
    let mut comp : Array Nat := Array.mkArray total 0
    let mut visited2 : Array Bool := Array.mkArray total false
    let mut cid : Nat := 0
    for i in [:total] do
      let v := order.get! (total - 1 - i)
      if !(visited2.get! v) then
        cid := cid + 1
        let mut stack : List Nat := [v]
        visited2 := visited2.set! v true
        while stack ≠ [] do
          match stack with
          | x :: rest =>
            comp := comp.set! x cid
            stack := rest
            for u in ts.radj.get! x do
              if !(visited2.get! u) then
                visited2 := visited2.set! u true
                stack := u :: stack
          | [] => pure ()
    let vars := ts.n
    let mut ok := true
    for i in [:vars] do
      if comp.get! (2*i) == comp.get! (2*i+1) then
        ok := false
    return ok

end TwoSAT

open TwoSAT

@[inline] def isG1 (startG1 : Bool) (pos : Nat) : Bool :=
  if startG1 then
    pos % 2 == 0
  else
    pos % 2 == 1

partial def solve (n : Nat) (rules : Array (Array Nat)) (startG1 : Bool) : Bool :=
  let mut ts := mk n
  -- adjacency constraints
  if n > 1 then
    for i in [:n-1] do
      ts := ts.addOr (lit i false) (lit (i+1) false)
  -- rules
  for r in rules do
    let mut g1 : Array Nat := #[]
    let mut g2 : Array Nat := #[]
    for pos in r do
      if isG1 startG1 pos then
        g1 := g1.push pos
      else
        g2 := g2.push pos
    let total := r.size
    if total > 4 || g1.size > 2 || g2.size > 2 then
      return false
    if g1.size == 2 then
      ts := ts.addXor (g1.get! 0) (g1.get! 1)
    if g2.size == 2 then
      ts := ts.addXor (g2.get! 0) (g2.get! 1)
  ts.satisfiable

partial def process (h : IO.FS.Stream) (cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let rec readLineSkip : IO String := do
      let line := (← h.getLine?)
      match line with
      | some s =>
        if s.trim = "" then readLineSkip else pure s
      | none => pure ""
    let first := (← readLineSkip).trim
    if first = "" then
      pure ()
    else
      let parts := first.split (· = ' ') |>.filter (· ≠ "")
      let n := parts.get! 0 |>.toNat!
      let p := parts.get! 1 |>.toNat!
      let mut rules : Array (Array Nat) := Array.mkEmpty p
      for _ in [:p] do
        let line := (← h.getLine).trim
        let ws := line.split (· = ' ') |>.filter (· ≠ "")
        let b := ws.get! 0 |>.toNat!
        let mut arr : Array Nat := Array.mkEmpty b
        for j in [:b] do
          let pos := ws.get! (j+1) |>.toNat!
          arr := arr.push (pos - 1)
        rules := rules.push arr
      let ok := solve n rules true || solve n rules false
      if ok then
        IO.println "approved"
      else
        IO.println "rejected"
      process h (cases - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
