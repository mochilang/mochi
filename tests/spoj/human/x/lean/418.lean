/- Solution for SPOJ NECKLACE - Necklace
https://www.spoj.com/problems/NECKLACE/
-/
import Std
open Std

structure Pt where
  x : Float
  y : Float

def dist (a b : Pt) : Float :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  Float.sqrt (dx*dx + dy*dy)

def formatFloat (x : Float) : String :=
  let y := (Float.round (x * 1000000.0)) / 1000000.0
  let s := y.toString
  if s.endsWith ".0" then s.dropRight 2 else s

partial def solveCase (pts : Array Pt) : Array Float :=
  let n := pts.size
  if n = 0 then #[] else
    Id.run do
      -- distances between consecutive points
      let mut d : Array Float := Array.mkArray n 0.0
      for i in [0:n] do
        let a := pts.get! i
        let b := pts.get! ((i+1) % n)
        d := d.set! i (dist a b)
      -- build sign and constant arrays
      let mut s : Array Float := Array.mkArray n 0.0
      let mut c : Array Float := Array.mkArray n 0.0
      s := s.set! 0 1.0
      c := c.set! 0 0.0
      for i in [1:n] do
        s := s.set! i (- s.get! (i-1))
        c := c.set! i (d.get! (i-1) - c.get! (i-1))
      let cN1 := d.get! (n-1) - c.get! (n-1)
      let mut r : Array Float := Array.mkArray n 0.0
      if n % 2 == 1 then
        let x := cN1 / 2.0
        for i in [0:n] do
          let ri := s.get! i * x + c.get! i
          r := r.set! i ri
      else
        let mut L : Float := -1e100
        let mut U : Float := 1e100
        for i in [0:n] do
          let si := s.get! i
          let ci := c.get! i
          if si = 1.0 then
            if -ci > L then L := -ci
          else
            if ci < U then U := ci
        for i in [0:n] do
          for j in [i+1:n] do
            if !(j = i+1 || (i = 0 && j = n-1)) then
              let dij := dist (pts.get! i) (pts.get! j)
              let si := s.get! i
              let sj := s.get! j
              if si + sj = 2.0 then
                let bound := (dij - c.get! i - c.get! j) / 2.0
                if bound < U then U := bound
              else if si + sj = -2.0 then
                let bound := (c.get! i + c.get! j - dij) / 2.0
                if bound > L then L := bound
              else
                if c.get! i + c.get! j > dij then
                  pure ()
        let x := (L + U) / 2.0
        for i in [0:n] do
          let ri := s.get! i * x + c.get! i
          r := r.set! i ri
      return r

partial def readCases (toks : Array String) (idx : Nat)
    (t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let mut pts : Array Pt := #[]
    let mut j := idx + 1
    for _ in [0:n] do
      let x := toks.get! j |>.toFloat!
      let y := toks.get! (j+1) |>.toFloat!
      pts := pts.push {x := x, y := y}
      j := j + 2
    let radii := solveCase pts
    let lines := radii.toList.map formatFloat
    readCases toks j (t-1) (lines ++ acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := readCases toks 1 t []
  for line in outs do
    IO.println line
