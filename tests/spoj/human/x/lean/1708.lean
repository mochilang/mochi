/- Solution for SPOJ SQCOUNT - Square Count
https://www.spoj.com/problems/SQCOUNT/
-/

import Std
open Std

structure Rect where
  x1 : Int
  y1 : Int
  x2 : Int
  y2 : Int

def Rect.mkOrdered (a b c d : Int) : Rect :=
  let x1 := min a c
  let x2 := max a c
  let y1 := min b d
  let y2 := max b d
  {x1 := x1, y1 := y1, x2 := x2, y2 := y2}

def width (r : Rect) : Int := r.x2 - r.x1
def height (r : Rect) : Int := r.y2 - r.y1

def countInRoom (r : Rect) : Int :=
  let w := (width r).toNat
  let h := (height r).toNat
  let m := Nat.min w h
  let mut acc : Int := 0
  for s in [1:m+1] do
    let add := (Int.ofNat (w - s + 1)) * (Int.ofNat (h - s + 1))
    acc := acc + add
  acc

def crossPair (a b : Rect) : Int :=
  let mut res : Int := 0
  -- vertical adjacency
  if a.x2 == b.x1 || b.x2 == a.x1 then
    let (L,R,c) :=
      if a.x2 == b.x1 then (a,b,a.x2) else (b,a,b.x2)
    let yA := max L.y1 R.y1
    let yB := min L.y2 R.y2
    let m := yB - yA
    if m > 2 then
      let doorLen := m - 2
      let wL := width L
      let wR := width R
      let DL : Nat := Int.toNat doorLen
      for snat in [1:DL+1] do
        let s : Int := Int.ofNat snat
        if s ≤ wL && s ≤ wR then
          let yPos := doorLen - s + 1
          let xLow := max L.x1 (c - s + 1)
          let xHigh := min (c - 1) (R.x2 - s)
          if xHigh ≥ xLow then
            res := res + yPos * (xHigh - xLow + 1)
  -- horizontal adjacency
  if a.y2 == b.y1 || b.y2 == a.y1 then
    let (B,T,c) :=
      if a.y2 == b.y1 then (a,b,a.y2) else (b,a,b.y2)
    let xA := max B.x1 T.x1
    let xB := min B.x2 T.x2
    let m := xB - xA
    if m > 2 then
      let doorLen := m - 2
      let hB := height B
      let hT := height T
      let DL : Nat := Int.toNat doorLen
      for snat in [1:DL+1] do
        let s : Int := Int.ofNat snat
        if s ≤ hB && s ≤ hT then
          let xPos := doorLen - s + 1
          let yLow := max B.y1 (c - s + 1)
          let yHigh := min (c - 1) (T.y2 - s)
          if yHigh ≥ yLow then
            res := res + xPos * (yHigh - yLow + 1)
  res

def countTotal (rooms : Array Rect) : Int :=
  let mut total : Int := 0
  for r in rooms do
    total := total + countInRoom r
  let n := rooms.size
  for i in [0:n] do
    let ri := rooms[i]!
    for j in [i+1:n] do
      let rj := rooms[j]!
      total := total + crossPair ri rj
  total

partial def process (toks : Array String) (idx case : Nat) : IO Unit := do
  if h : idx < toks.size then
    let n := (toks.get! idx).toNat!
    if n == 0 then
      pure ()
    else
      let mut rooms : Array Rect := Array.mkEmpty n
      let mut j := idx + 1
      for _ in [0:n] do
        let x1 := (toks.get! j).toInt!
        let y1 := (toks.get! (j+1)).toInt!
        let x2 := (toks.get! (j+2)).toInt!
        let y2 := (toks.get! (j+3)).toInt!
        rooms := rooms.push (Rect.mkOrdered x1 y1 x2 y2)
        j := j + 4
      let ans := countTotal rooms
      IO.println s!"Case {case}: {ans}"
      process toks j (case + 1)
  else
    pure ()

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "") |>.toArray
  process toks 0 1
