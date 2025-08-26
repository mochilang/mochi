/- Solution for SPOJ MOULDS - Moulds
https://www.spoj.com/problems/MOULDS/
-/

import Std
open Std

structure Ctx where
  x : Int
  y : Int
  z : Int
  grid : Std.HashMap (Int × Int) Int

abbrev M := StateM Ctx

private partial def parseInt (s : String) (i : Nat) : Int × Nat :=
  let len := s.length
  let (sign, j0) :=
    if h : i < len then
      let c := s.get ⟨i, h⟩
      if c == '-' then (-1, i+1)
      else if c == '+' then (1, i+1)
      else (1, i)
    else (1, i)
  let rec loop (j : Nat) (acc : Int) : Int × Nat :=
    if h : j < len then
      let c := s.get ⟨j, h⟩
      if c.isDigit then
        let d := Int.ofNat (c.toNat - '0'.toNat)
        loop (j+1) (acc * 10 + d)
      else
        (acc, j)
    else
      (acc, j)
  let (n, j) := loop j0 0
  (sign * n, j)

mutual
  partial def parseBlock (s : String) (i : Nat) : M Nat := do
    let rec loop (j : Nat) : M Nat := do
      if s.get! j == ']' then
        pure (j+1)
      else
        let j ← parseCmd s j
        loop (j+1)
    loop (i+1)

  partial def parseCmd (s : String) (i : Nat) : M Nat := do
    match s.get! i with
    | '[' => parseBlock s i
    | '^' => do
        let (d, j) := parseInt s (i+1)
        let ctx ← get
        let newZ := ctx.z - d
        let mut grid := ctx.grid
        if newZ < ctx.z && newZ < 0 &&
            0 ≤ ctx.x && ctx.x < 250 && 0 ≤ ctx.y && ctx.y < 250 then
          let old := grid.findD (ctx.x, ctx.y) 0
          grid := grid.insert (ctx.x, ctx.y) (min old newZ)
        set { ctx with z := newZ, grid := grid }
        pure j
    | '@' => do
        let dir := s.get! (i+1)
        let (dist, j) := parseInt s (i+2)
        let ctx ← get
        let (dx, dy) :=
          match dir with
          | 'N' => (0, -dist)
          | 'S' => (0, dist)
          | 'W' => (-dist, 0)
          | _   => (dist, 0) -- 'E'
        let steps : Nat := Int.natAbs dx + Int.natAbs dy
        let sx := if dx > 0 then 1 else if dx < 0 then -1 else 0
        let sy := if dy > 0 then 1 else if dy < 0 then -1 else 0
        let mut x := ctx.x
        let mut y := ctx.y
        let mut grid := ctx.grid
        for _ in [0:steps] do
          x := x + sx
          y := y + sy
          if ctx.z < 0 && 0 ≤ x && x < 250 && 0 ≤ y && y < 250 then
            let old := grid.findD (x, y) 0
            grid := grid.insert (x, y) (min old ctx.z)
        set { ctx with x := x, y := y, grid := grid }
        pure j
    | _ => pure i
end

def solve (s : String) : Int :=
  let ctx := { x := (0:Int), y := 0, z := 1, grid := {} }
  let (_, st) := (parseBlock s 0).run ctx
  st.grid.fold (init := 0) (fun acc _ v => acc + (-v))

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let line := (← h.getLine).trim
    let vol := solve line
    IO.println vol
