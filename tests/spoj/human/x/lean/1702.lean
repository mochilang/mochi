/- Solution for SPOJ CLEANRBT - Cleaning Robot
https://www.spoj.com/problems/CLEANRBT/
-/

import Std
open Std

structure Pos where
  x : Nat
  y : Nat
  deriving Inhabited

private def solveMap (w h : Nat) (rows : Array String) : Int :=
  Id.run do
    let wI : Int := Int.ofNat w
    let hI : Int := Int.ofNat h
    let mut board : Array (Array Char) := Array.mkArray h (Array.mkArray w ' ')
    let mut sx := 0
    let mut sy := 0
    let mut dirty : Array Pos := #[]
    for y in [0:h] do
      let arr := (rows[y]!).data.toArray
      board := board.set! y arr
      for x in [0:w] do
        let c := arr[x]!
        if c = '*' then
          dirty := dirty.push {x := x, y := y}
        else if c = 'o' then
          sx := x
          sy := y
    let k := dirty.size
    let mut idxMap : Array Nat := Array.mkArray (w * h) k
    for i in [0:k] do
      let p := dirty[i]!
      idxMap := idxMap.set! (p.y * w + p.x) i
    let maskCount := (1 <<< k)
    let allMask := maskCount - 1
    let total := w * h * maskCount
    let mut visited : Array Bool := Array.mkArray total false
    let index := fun (x y m : Nat) => ((y * w + x) * maskCount) + m
    let mut q : Std.Queue (Nat × Nat × Nat × Nat) := .empty
    let startIdx := index sx sy 0
    visited := visited.set! startIdx true
    q := q.enqueue (sx, sy, 0, 0)
    let dirs : Array (Int × Int) := #[(1,0),(-1,0),(0,1),(0,-1)]
    while true do
      match q.dequeue? with
      | none =>
          return (-1)
      | some ((x, y, mask, d), q') =>
          q := q'
          if mask = allMask then
            return Int.ofNat d
          for dir in dirs do
            let nxI := Int.ofNat x + dir.fst
            let nyI := Int.ofNat y + dir.snd
            if decide (0 ≤ nxI) && decide (nxI < wI) && decide (0 ≤ nyI) && decide (nyI < hI) then
              let nx := Int.toNat nxI
              let ny := Int.toNat nyI
              if (board[ny]!)[nx]! ≠ 'x' then
                let di := idxMap[(ny * w + nx)]
                let newMask := if di < k then mask ||| (1 <<< di) else mask
                let idx := index nx ny newMask
                if !visited[idx]! then
                  visited := visited.set! idx true
                  q := q.enqueue (nx, ny, newMask, d + 1)
    return (-1)

partial def parseAll (toks : Array String) (idx : Nat) (acc : List Int) : List Int :=
  if idx + 1 ≥ toks.size then acc.reverse else
    let w := toks[idx]!.toNat!
    let h := toks[idx+1]!.toNat!
    if w = 0 && h = 0 then acc.reverse else
      let mut rows : Array String := Array.mkArray h ""
      for r in [0:h] do
        rows := rows.set! r (toks[idx + 2 + r]!)
      let res := solveMap w h rows
      parseAll toks (idx + 2 + h) (res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "") |> List.toArray
  let outs := parseAll toks 0 []
  for (i, v) in outs.enum do
    IO.println v
