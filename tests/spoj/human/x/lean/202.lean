/- Solution for SPOJ ROCKETS - Rockets
https://www.spoj.com/problems/ROCKETS/
-/

import Std
open Std

structure P where
  x : Int
  y : Int
  idx : Nat
  deriving Inhabited

private def orient (a b c : P) : Int :=
  (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

partial def matchPairs (rs ws : Array P) (ans : Array Nat) : Array Nat :=
  if rs.size == 1 then
    ans.set! rs[0]!.idx (ws[0]!.idx + 1)
  else
    let r0 := rs.get! 0
    -- find matching white point
    let mut j := 0
    let mut found := 0
    while j < ws.size do
      let wj := ws.get! j
      -- count points on left side of r0-wj
      let mut cr := 0
      let mut i := 1
      while i < rs.size do
        let ri := rs.get! i
        if orient r0 wj ri > 0 then
          cr := cr + 1
        i := i + 1
      let mut cw := 0
      let mut k := 0
      while k < ws.size do
        if k != j then
          let wk := ws.get! k
          if orient r0 wj wk > 0 then
            cw := cw + 1
        k := k + 1
      if cr == cw then
        found := j
        j := ws.size
      else
        j := j + 1
    let w0 := ws.get! found
    -- partition points around line r0-w0
    let mut rsL : Array P := Array.mkEmpty rs.size
    let mut rsR : Array P := Array.mkEmpty rs.size
    let mut wsL : Array P := Array.mkEmpty ws.size
    let mut wsR : Array P := Array.mkEmpty ws.size
    let mut i := 1
    while i < rs.size do
      let ri := rs.get! i
      if orient r0 w0 ri > 0 then
        rsL := rsL.push ri
      else
        rsR := rsR.push ri
      i := i + 1
    let mut k := 0
    while k < ws.size do
      if k != found then
        let wk := ws.get! k
        if orient r0 w0 wk > 0 then
          wsL := wsL.push wk
        else
          wsR := wsR.push wk
      k := k + 1
    let ans := ans.set! r0.idx (w0.idx + 1)
    let ans := matchPairs rsL wsL ans
    let ans := matchPairs rsR wsR ans
    ans

partial def solve (tokens : Array String) (idx t : Nat) : IO Unit :=
  if t == 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let mut pos := idx + 1
    let mut rs : Array P := Array.mkEmpty n
    let mut i := 0
    while i < n do
      let x := (tokens.get! pos).toInt!
      let y := (tokens.get! (pos+1)).toInt!
      rs := rs.push {x:=x, y:=y, idx:=i}
      i := i + 1
      pos := pos + 2
    let mut ws : Array P := Array.mkEmpty n
    let mut j := 0
    while j < n do
      let x := (tokens.get! pos).toInt!
      let y := (tokens.get! (pos+1)).toInt!
      ws := ws.push {x:=x, y:=y, idx:=j}
      j := j + 1
      pos := pos + 2
    let ans := matchPairs rs ws (Array.mkArray n 0)
    let mut k := 0
    while k < n do
      IO.println (ans.get! k)
      k := k + 1
    solve tokens pos (t-1)

def main : IO Unit := do
  let data <- IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
