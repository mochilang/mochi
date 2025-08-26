/- Solution for SPOJ RUNAWAY - Run Away
https://www.spoj.com/problems/RUNAWAY/
-/

import Std
open Std

structure Point where
  x : Float
  y : Float

def distance (a b : Point) : Float :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  Float.sqrt (dx*dx + dy*dy)

def nearestDist (p : Point) (holes : Array Point) : Float :=
  holes.foldl (init := 1e18) (fun acc h => Float.min acc (distance p h))

def inside (p : Point) (w h : Float) : Bool :=
  p.x ≥ 0.0 && p.x ≤ w && p.y ≥ 0.0 && p.y ≤ h

def dirs : Array Point := #[
  ⟨1,0⟩, ⟨-1,0⟩, ⟨0,1⟩, ⟨0,-1⟩,
  ⟨1,1⟩, ⟨1,-1⟩, ⟨-1,1⟩, ⟨-1,-1⟩
]

partial def climb (p : Point) (step w h : Float) (holes : Array Point) : Point :=
  if step < 0.01 then p else
    let base := nearestDist p holes
    let (bestP, bestD) := dirs.foldl
      (fun (acc : Point × Float) d =>
        let np := { x := p.x + d.x*step, y := p.y + d.y*step }
        if inside np w h then
          let nd := nearestDist np holes
          if nd > acc.snd then (np, nd) else acc
        else acc
      ) (p, base)
    if bestD > base then
      climb bestP step w h holes
    else
      climb p (step/2.0) w h holes

def format1 (x : Float) : String :=
  let r := (Float.round (x * 10.0)) / 10.0
  let s := r.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      (parts.get! 1 ++ "0").take 1
    else
      "0"
  intPart ++ "." ++ fracPart

partial def readHoles (toks : Array String) (idx m : Nat) (acc : Array Point) :
    (Array Point × Nat) :=
  if m = 0 then (acc, idx) else
    let x := toks.get! idx |>.toFloat!
    let y := toks.get! (idx+1) |>.toFloat!
    readHoles toks (idx+2) (m-1) (acc.push ⟨x, y⟩)

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) :
    List String :=
  if t = 0 then acc.reverse else
    let w := toks.get! idx |>.toFloat!
    let h := toks.get! (idx+1) |>.toFloat!
    let m := toks.get! (idx+2) |>.toNat!
    let (holes, idx') := readHoles toks (idx+3) m #[]
    let step := Float.max w h
    let starts := holes ++ # [⟨0,0⟩, ⟨w,0⟩, ⟨0,h⟩, ⟨w,h⟩]
    let (ans, _) := starts.foldl
      (fun (accP : Point × Float) s =>
        let p := climb s step w h holes
        let d := nearestDist p holes
        if d > accP.snd then (p, d) else accP
      ) ({x := 0.0, y := 0.0}, -1.0)
    let line := s!"The safest point is ({format1 ans.x}, {format1 ans.y})."
    solveAll toks idx' (t-1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (fun s => s ≠ "")
                  |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 t []
  for line in outs do
    IO.println line
