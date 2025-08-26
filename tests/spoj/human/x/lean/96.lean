/- Solution for SPOJ SHOP - Shopping
https://www.spoj.com/problems/SHOP/
-/
import Std
open Std

abbrev Pos := Nat × Nat

def dijkstra (grid : Array (Array (Option Nat))) (start dest : Pos) (w h : Nat) : Nat :=
  Id.run do
    let size := w * h
    let idx := fun (x y : Nat) => y * w + x
    let mut dist := Array.mkArray size (1000000000)
    let mut used := Array.mkArray size false
    dist := dist.set! (idx start.fst start.snd) 0
    for _ in [0:size] do
      -- find unvisited node with smallest distance
      let mut u := size
      let mut best := 1000000000
      for i in [0:size] do
        if !(used.get! i) then
          let d := dist.get! i
          if d < best then
            best := d
            u := i
      if u == size then
        pure ()
      else
        used := used.set! u true
        let ux := u % w
        let uy := u / w
        let cur := dist.get! u
        -- left
        if ux > 0 then
          let vx := ux - 1
          let vy := uy
          match grid.get! vy |>.get! vx with
          | some c =>
              let vIdx := idx vx vy
              let nd := cur + c
              if nd < dist.get! vIdx then
                dist := dist.set! vIdx nd
          | none => pure ()
        -- right
        if ux + 1 < w then
          let vx := ux + 1
          let vy := uy
          match grid.get! vy |>.get! vx with
          | some c =>
              let vIdx := idx vx vy
              let nd := cur + c
              if nd < dist.get! vIdx then
                dist := dist.set! vIdx nd
          | none => pure ()
        -- up
        if uy > 0 then
          let vx := ux
          let vy := uy - 1
          match grid.get! vy |>.get! vx with
          | some c =>
              let vIdx := idx vx vy
              let nd := cur + c
              if nd < dist.get! vIdx then
                dist := dist.set! vIdx nd
          | none => pure ()
        -- down
        if uy + 1 < h then
          let vx := ux
          let vy := uy + 1
          match grid.get! vy |>.get! vx with
          | some c =>
              let vIdx := idx vx vy
              let nd := cur + c
              if nd < dist.get! vIdx then
                dist := dist.set! vIdx nd
          | none => pure ()
    dist.get! (idx dest.fst dest.snd)

partial def solve (toks : Array String) (i : Nat) (acc : List Nat) : List Nat :=
  let w := toks.get! i |>.toNat!
  let h := toks.get! (i+1) |>.toNat!
  if w == 0 && h == 0 then acc.reverse else
    let mut grid : Array (Array (Option Nat)) := Array.mkArray h (Array.mkArray w none)
    let mut start : Pos := (0,0)
    let mut dest : Pos := (0,0)
    for y in [0:h] do
      let rowStr := toks.get! (i + 2 + y)
      let mut row := Array.mkArray w none
      for x in [0:w] do
        let ch := rowStr.get! x
        if ch == 'X' then
          row := row.set! x none
        else if ch == 'S' then
          row := row.set! x (some 0)
          start := (x, y)
        else if ch == 'D' then
          row := row.set! x (some 0)
          dest := (x, y)
        else
          let c := ch.toNat - '0'.toNat
          row := row.set! x (some c)
      grid := grid.set! y row
    let ans := dijkstra grid start dest w h
    solve toks (i + 2 + h) (ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let res := solve toks 0 []
  for v in res do
    IO.println v
