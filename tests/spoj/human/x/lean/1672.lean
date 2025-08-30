/- Solution for SPOJ GIWED - The Great Indian Wedding
https://www.spoj.com/problems/GIWED/
-/
import Std
open Std

/-- Return sorted array with duplicates removed. -/
def uniqSorted (xs : Array Nat) : Array Nat :=
  Id.run do
    let mut arr := xs.qsort (fun a b => a < b)
    let mut res : Array Nat := Array.mkEmpty arr.size
    let mut last : Nat := 0
    let mut first := true
    for x in arr do
      if first || x ≠ last then
        res := res.push x
        last := x
        first := false
    return res

/-- Find index of value `v` in array `arr`. Assumes `v` is present. -/
def indexOf! (arr : Array Nat) (v : Nat) : Nat :=
  Id.run do
    for i in [0:arr.size] do
      if arr.get! i == v then
        return i
    return 0

/-- Compute maximum watered area. -/
def compute (M N L : Nat) (carpets : Array (Nat × Nat × Nat × Nat)) : Nat :=
  Id.run do
    -- coordinate compression
    let mut xsList : Array Nat := #[0, M]
    let mut ysList : Array Nat := #[0, N]
    for c in carpets do
      let (x1, y1, x2, y2) := c
      xsList := xsList.push x1; xsList := xsList.push x2
      ysList := ysList.push y1; ysList := ysList.push y2
    let xs := uniqSorted xsList
    let ys := uniqSorted ysList
    let w := xs.size - 1
    let h := ys.size - 1
    -- blocked grid
    let mut blocked : Array (Array Bool) := Array.mkArray h (Array.mkArray w false)
    for c in carpets do
      let (x1, y1, x2, y2) := c
      let ix1 := indexOf! xs x1
      let ix2 := indexOf! xs x2
      let iy1 := indexOf! ys y1
      let iy2 := indexOf! ys y2
      for y in [iy1:iy2] do
        let row := blocked.get! y
        let mut row2 := row
        for x in [ix1:ix2] do
          row2 := row2.set! x true
        blocked := blocked.set! y row2
    -- visited grid
    let mut vis : Array (Array Bool) := Array.mkArray h (Array.mkArray w false)
    let mut areas : Array Nat := #[]
    for y in [0:h] do
      for x in [0:w] do
        if !(blocked.get! y |>.get! x) && !(vis.get! y |>.get! x) then
          vis := vis.set! y ((vis.get! y).set! x true)
          let mut stack : List (Nat × Nat) := [(x,y)]
          let mut a : Nat := 0
          while stack ≠ [] do
            let (cx, cy) := stack.head!
            stack := stack.tail!
            a := a + (xs.get! (cx+1) - xs.get! cx) * (ys.get! (cy+1) - ys.get! cy)
            if cx > 0 then
              let nx := cx - 1; let ny := cy
              if !(blocked.get! ny |>.get! nx) && !(vis.get! ny |>.get! nx) then
                vis := vis.set! ny ((vis.get! ny).set! nx true)
                stack := (nx, ny) :: stack
            if cx + 1 < w then
              let nx := cx + 1; let ny := cy
              if !(blocked.get! ny |>.get! nx) && !(vis.get! ny |>.get! nx) then
                vis := vis.set! ny ((vis.get! ny).set! nx true)
                stack := (nx, ny) :: stack
            if cy > 0 then
              let nx := cx; let ny := cy - 1
              if !(blocked.get! ny |>.get! nx) && !(vis.get! ny |>.get! nx) then
                vis := vis.set! ny ((vis.get! ny).set! nx true)
                stack := (nx, ny) :: stack
            if cy + 1 < h then
              let nx := cx; let ny := cy + 1
              if !(blocked.get! ny |>.get! nx) && !(vis.get! ny |>.get! nx) then
                vis := vis.set! ny ((vis.get! ny).set! nx true)
                stack := (nx, ny) :: stack
          areas := areas.push a
    let sorted := areas.qsort (fun a b => b < a)
    let take := if sorted.size < L then sorted.size else L
    let mut ans : Nat := 0
    for i in [0:take] do
      ans := ans + sorted.get! i
    return ans

/-- Main IO: parse input and output result. -/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks : Array String :=
    (data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
         |>.filter (fun s => s ≠ "")).toArray
  let mut i := 0
  let T := toks[i]!.toNat!; i := i + 1
  let mut res : Array Nat := #[]
  for _ in [0:T] do
    let M := toks[i]!.toNat!; i := i + 1
    let N := toks[i]!.toNat!; i := i + 1
    let K := toks[i]!.toNat!; i := i + 1
    let L := toks[i]!.toNat!; i := i + 1
    let mut carpets : Array (Nat × Nat × Nat × Nat) := Array.mkEmpty K
    for _ in [0:K] do
      let x1 := toks[i]!.toNat!; i := i + 1
      let y1 := toks[i]!.toNat!; i := i + 1
      let x2 := toks[i]!.toNat!; i := i + 1
      let y2 := toks[i]!.toNat!; i := i + 1
      carpets := carpets.push (x1, y1, x2, y2)
    res := res.push (compute M N L carpets)
  for v in res do
    IO.println v
