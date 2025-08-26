/- Solution for SPOJ BAC - Bacterial
https://www.spoj.com/problems/BAC/
-/

import Std
open Std

structure Pt where
  x : Float
  y : Float

def repeatChar (c : Char) (n : Nat) : String :=
  String.mk (List.replicate n c)

def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let l := s.length
  if l >= len then s else repeatChar c (len - l) ++ s

def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

def polygonArea (poly : List Pt) : Float :=
  if poly.length < 3 then 0.0 else
    let arr := poly.toArray
    let n := arr.size
    Id.run do
      let mut s : Float := 0.0
      for i in [0:n] do
        let p := arr.get! i
        let q := arr.get! ((i+1) % n)
        s := s + (p.x * q.y - q.x * p.y)
      return Float.abs s / 2.0

def clipHalfPlane (poly : List Pt) (A B C : Float) : List Pt :=
  if poly.isEmpty then [] else
    let arr := poly.toArray
    let n := arr.size
    Id.run do
      let mut res : List Pt := []
      for i in [0:n] do
        let curr := arr.get! i
        let prev := arr.get! ((i + n - 1) % n)
        let c1 := A * prev.x + B * prev.y + C
        let c2 := A * curr.x + B * curr.y + C
        let inPrev := c1 <= 0.0
        let inCurr := c2 <= 0.0
        if inPrev && inCurr then
          res := curr :: res
        else if inPrev && !inCurr then
          let t := c1 / (c1 - c2)
          let ix := prev.x + (curr.x - prev.x) * t
          let iy := prev.y + (curr.y - prev.y) * t
          res := {x := ix, y := iy} :: res
        else if !inPrev && inCurr then
          let t := c1 / (c1 - c2)
          let ix := prev.x + (curr.x - prev.x) * t
          let iy := prev.y + (curr.y - prev.y) * t
          res := curr :: {x := ix, y := iy} :: res
        else
          pure ()
      return res.reverse

def areaFor (pts : Array Pt) (i : Nat) (w h : Float) : Float :=
  let pi := pts.get! i
  Id.run do
    let mut poly : List Pt := [{x := 0.0, y := 0.0}, {x := w, y := 0.0}, {x := w, y := h}, {x := 0.0, y := h}]
    for j in [0:pts.size] do
      if j != i then
        let pj := pts.get! j
        let A := pj.x - pi.x
        let B := pj.y - pi.y
        let midx := (pi.x + pj.x) / 2.0
        let midy := (pi.y + pj.y) / 2.0
        let C := - (A * midx + B * midy)
        poly := clipHalfPlane poly A B C
    return polygonArea poly

def solveCase (w h : Float) (pts : Array Pt) : List String :=
  let n := pts.size
  let mut arr : Array (Nat × Float) := #[]
  for i in [0:n] do
    let area := areaFor pts i w h
    arr := arr.push (i+1, area)
  let lst := arr.toList.sort (fun a b => a.snd > b.snd)
  lst.map (fun (id, area) =>
    let idStr := padLeft (toString id) 3 '0'
    let areaStr := padLeft (format2 area) 14 ' '
    idStr ++ areaStr)

partial def process (toks : Array String) (idx : Nat) : IO Unit := do
  if idx ≥ toks.size then
    pure ()
  else
    let w := (toks.get! idx).toNat!
    let h := (toks.get! (idx+1)).toNat!
    if w = 0 && h = 0 then
      pure ()
    else
      let n := (toks.get! (idx+2)).toNat!
      let mut pts : Array Pt := #[]
      let mut j := idx + 3
      for _ in [0:n] do
        let x := (toks.get! j).toNat!
        let y := (toks.get! (j+1)).toNat!
        pts := pts.push {x := Float.ofNat x, y := Float.ofNat y}
        j := j + 2
      let lines := solveCase (Float.ofNat w) (Float.ofNat h) pts
      for line in lines do
        IO.println line
      IO.println ""
      process toks j

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  process toks 0
