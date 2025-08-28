/- Solution for SPOJ SCANNER - Scanner
https://www.spoj.com/problems/SCANNER/
-/

import Std
open Std

abbrev Grid := Array (Array Int)

-- build cell lists for diagonals
private def diag1Cells (r c : Nat) : Array (Array (Nat × Nat)) :=
  Id.run do
    let mut arr : Array (Array (Nat × Nat)) := Array.mkArray (r+c-1) #[]
    for i in [0:r] do
      for j in [0:c] do
        let k := i + j
        let list := arr.get! k
        arr := arr.set! k (list.push (i,j))
    return arr

private def diag2Cells (r c : Nat) : Array (Array (Nat × Nat)) :=
  Id.run do
    let mut arr : Array (Array (Nat × Nat)) := Array.mkArray (r+c-1) #[]
    for i in [0:r] do
      for j in [0:c] do
        let k := j + (r-1) - i
        let list := arr.get! k
        arr := arr.set! k (list.push (i,j))
    return arr

-- propagate deterministic constraints
private def propagate (grid : Grid) (rowLeft rowRem colLeft colRem d1Left d1Rem d2Left d2Rem : Array Int)
    (cells1 cells2 : Array (Array (Nat × Nat))) : (Grid × Array Int × Array Int × Array Int × Array Int × Array Int × Array Int × Array Int × Array Int) :=
  Id.run do
    let r := grid.size
    let c := grid.get! 0 |>.size
    let mut g := grid
    let mut rLeft := rowLeft
    let mut rRem := rowRem
    let mut cLeft := colLeft
    let mut cRem := colRem
    let mut d1L := d1Left
    let mut d1R := d1Rem
    let mut d2L := d2Left
    let mut d2R := d2Rem
    let mut changed := true
    while changed do
      changed := false
      -- rows
      for i in [0:r] do
        if rRem.get! i > 0 then
          let rl := rLeft.get! i
          let rr := rRem.get! i
          if rl == 0 then
            for j in [0:c] do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 0)
                rRem := rRem.set! i (rRem.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                changed := true
          else if rl == rr then
            for j in [0:c] do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 1)
                rRem := rRem.set! i (rRem.get! i - 1)
                rLeft := rLeft.set! i (rLeft.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                cLeft := cLeft.set! j (cLeft.get! j - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                d1L := d1L.set! k1 (d1L.get! k1 - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                d2L := d2L.set! k2 (d2L.get! k2 - 1)
                changed := true
      -- columns
      for j in [0:c] do
        if cRem.get! j > 0 then
          let cl := cLeft.get! j
          let cr := cRem.get! j
          if cl == 0 then
            for i in [0:r] do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 0)
                rRem := rRem.set! i (rRem.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                changed := true
          else if cl == cr then
            for i in [0:r] do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 1)
                rRem := rRem.set! i (rRem.get! i - 1)
                rLeft := rLeft.set! i (rLeft.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                cLeft := cLeft.set! j (cLeft.get! j - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                d1L := d1L.set! k1 (d1L.get! k1 - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                d2L := d2L.set! k2 (d2L.get! k2 - 1)
                changed := true
      -- diag1
      let total := r + c - 1
      for k in [0:total] do
        if d1R.get! k > 0 then
          let dl := d1L.get! k
          let dr := d1R.get! k
          if dl == 0 then
            for (i,j) in cells1.get! k do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 0)
                rRem := rRem.set! i (rRem.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                d1R := d1R.set! k (d1R.get! k - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                changed := true
          else if dl == dr then
            for (i,j) in cells1.get! k do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 1)
                rRem := rRem.set! i (rRem.get! i - 1)
                rLeft := rLeft.set! i (rLeft.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                cLeft := cLeft.set! j (cLeft.get! j - 1)
                d1R := d1R.set! k (d1R.get! k - 1)
                d1L := d1L.set! k (d1L.get! k - 1)
                let k2 := j + (r-1) - i
                d2R := d2R.set! k2 (d2R.get! k2 - 1)
                d2L := d2L.set! k2 (d2L.get! k2 - 1)
                changed := true
      -- diag2
      for k in [0:total] do
        if d2R.get! k > 0 then
          let dl := d2L.get! k
          let dr := d2R.get! k
          if dl == 0 then
            for (i,j) in cells2.get! k do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 0)
                rRem := rRem.set! i (rRem.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                d2R := d2R.set! k (d2R.get! k - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                changed := true
          else if dl == dr then
            for (i,j) in cells2.get! k do
              if g.get! i |>.get! j == -1 then
                g := g.set! i (g.get! i |>.set! j 1)
                rRem := rRem.set! i (rRem.get! i - 1)
                rLeft := rLeft.set! i (rLeft.get! i - 1)
                cRem := cRem.set! j (cRem.get! j - 1)
                cLeft := cLeft.set! j (cLeft.get! j - 1)
                d2R := d2R.set! k (d2R.get! k - 1)
                d2L := d2L.set! k (d2L.get! k - 1)
                let k1 := i + j
                d1R := d1R.set! k1 (d1R.get! k1 - 1)
                d1L := d1L.set! k1 (d1L.get! k1 - 1)
                changed := true
    return (g, rLeft, rRem, cLeft, cRem, d1L, d1R, d2L, d2R)

-- solve one slice
private def solveSlice (rows : Array Int) (diag1 : Array Int) (cols : Array Int) (diag2 : Array Int) : Array String :=
  Id.run do
    let r := rows.size
    let c := cols.size
    let cells1 := diag1Cells r c
    let cells2 := diag2Cells r c
    let mut grid : Grid := Array.mkArray r (Array.mkArray c (-1))
    let mut rowLeft := rows
    let mut rowRem := Array.mkArray r c
    let mut colLeft := cols
    let mut colRem := Array.mkArray c r
    let total := r + c - 1
    let mut d1Left := diag1
    let mut d1Rem := Array.mkArray total 0
    for k in [0:total] do
      d1Rem := d1Rem.set! k (cells1.get! k |>.size)
    let mut d2Left := diag2
    let mut d2Rem := Array.mkArray total 0
    for k in [0:total] do
      d2Rem := d2Rem.set! k (cells2.get! k |>.size)
    let (g, rl, rr, cl, cr, d1l, d1r, d2l, d2r) :=
      propagate grid rowLeft rowRem colLeft colRem d1Left d1Rem d2Left d2Rem cells1 cells2
    -- build output strings
    let mut lines : Array String := Array.mkArray r ""
    for i in [0:r] do
      let mut s := ""
      for j in [0:c] do
        let v := g.get! i |>.get! j
        let ch := if v == 1 then '#' else '.'
        s := s.push ch
      lines := lines.set! i s
    return lines

-- parse all ints from stdin
private def readInts (h : IO.FS.Stream) : IO (Array Int) := do
  let data ← h.readToEnd
  let toks := data.toString.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "")
  let arr := toks.map (fun s => s.toInt!) |>.toArray
  return arr

partial def process (toks : Array Int) (idx : Nat) (t : Nat) (acc : Array String) : Array String :=
  if t == 0 then
    acc
  else
    let rows := toks.extract idx (idx+10) |>.map id
    let diag1 := toks.extract (idx+10) (idx+34)
    let cols := toks.extract (idx+34) (idx+49)
    let diag2 := toks.extract (idx+49) (idx+73)
    let lines := solveSlice rows diag1 cols diag2
    let acc := acc ++ lines.push ""
    process toks (idx+73) (t-1) acc

def main : IO Unit := do
  let h ← IO.getStdin
  let ints ← readInts h
  let t := ints.get! 0 |>.toNat
  let lines := process ints 1 t #[]
  for s in lines.dropLast? |>.getD #[] do
    if s == "" then
      IO.println ""
    else
      IO.println s
