/- Solution for SPOJ CAVE - Cave Exploration
https://www.spoj.com/problems/CAVE/
-/

import Std
open Std

private def simulate (hId : Array Nat) (hY hX1 hX2 : Array Int)
    (hPts : Array (List Int))
    (vId : Array Nat) (vX vY1 vY2 : Array Int)
    (vPts : Array (List Int))
    (entryX entryY : Int) (startDir corrTotal : Nat) : Nat :=
  Id.run do
    let mut nodeMap : Std.HashMap (Int × Int) Nat := {}
    let mut xs : Array Int := #[]
    let mut ys : Array Int := #[]
    let mut north : Array (Option (Nat × Nat)) := #[]
    let mut east  : Array (Option (Nat × Nat)) := #[]
    let mut south : Array (Option (Nat × Nat)) := #[]
    let mut west  : Array (Option (Nat × Nat)) := #[]
    let getNode := fun (x : Int) (y : Int) =>
      match nodeMap.find? (x, y) with
      | some idx => idx
      | none =>
        let idx := xs.size
        nodeMap := nodeMap.insert (x, y) idx
        xs := xs.push x
        ys := ys.push y
        north := north.push none
        east := east.push none
        south := south.push none
        west := west.push none
        idx
    -- build horizontal edges
    for i in [0:hId.size] do
      let y := hY.get! i
      let arr := ((hPts.get! i).eraseDups.qsort (· < ·)).toArray
      for j in [0:arr.size-1] do
        let xa := arr.get! j
        let xb := arr.get! (j+1)
        let nA := getNode xa y
        let nB := getNode xb y
        east := east.set! nA (some (nB, hId.get! i))
        west := west.set! nB (some (nA, hId.get! i))
    -- build vertical edges
    for i in [0:vId.size] do
      let x := vX.get! i
      let arr := ((vPts.get! i).eraseDups.qsort (· < ·)).toArray
      for j in [0:arr.size-1] do
        let ya := arr.get! j
        let yb := arr.get! (j+1)
        let nA := getNode x ya
        let nB := getNode x yb
        north := north.set! nA (some (nB, vId.get! i))
        south := south.set! nB (some (nA, vId.get! i))
    let getEdge := fun (node : Nat) (dir : Nat) =>
      match dir with
      | 0 => north.get! node
      | 1 => east.get! node
      | 2 => south.get! node
      | _ => west.get! node
    let startNode := getNode entryX entryY
    let mut cur := startNode
    let mut dir := startDir
    let mut visited : Array Bool := Array.mkArray corrTotal false
    let mut visits : Nat := 1
    while visits < 2 do
      let d1 := (dir + 3) % 4
      let (ndir, edge) :=
        match getEdge cur d1 with
        | some e => (d1, e)
        | none =>
          let d2 := dir
          match getEdge cur d2 with
          | some e => (d2, e)
          | none =>
            let d3 := (dir + 1) % 4
            match getEdge cur d3 with
            | some e => (d3, e)
            | none =>
              let d4 := (dir + 2) % 4
              (d4, (getEdge cur d4).get!)
      let nextNode := edge.fst
      let corr := edge.snd
      visited := visited.set! corr true
      cur := nextNode
      dir := ndir
      if cur == startNode then
        visits := visits + 1
    let mut res : Nat := 0
    for i in [0:corrTotal] do
      if (visited.get! i) = false then
        res := res + 1
    return res

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let toks := input.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r').filter (· ≠ "")
  let arr : Array String := toks.toArray
  let mut idx : Nat := 0
  let next := fun () =>
    let t := arr.get! idx
    idx := idx + 1
    t
  let t := (next ()).toNat!
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := (next ()).toNat!
    let mut hId : Array Nat := #[]
    let mut hY  : Array Int := #[]
    let mut hX1 : Array Int := #[]
    let mut hX2 : Array Int := #[]
    let mut hPts : Array (List Int) := #[]
    let mut vId : Array Nat := #[]
    let mut vX  : Array Int := #[]
    let mut vY1 : Array Int := #[]
    let mut vY2 : Array Int := #[]
    let mut vPts : Array (List Int) := #[]
    let mut cid : Nat := 0
    for _ in [0:n] do
      let typ := next()
      if typ = "H" then
        let y := (next()).toInt!
        let x1 := (next()).toInt!
        let x2 := (next()).toInt!
        hId := hId.push cid
        hY := hY.push y
        hX1 := hX1.push x1
        hX2 := hX2.push x2
        hPts := hPts.push [x1, x2]
      else
        let x := (next()).toInt!
        let y1 := (next()).toInt!
        let y2 := (next()).toInt!
        vId := vId.push cid
        vX := vX.push x
        vY1 := vY1.push y1
        vY2 := vY2.push y2
        vPts := vPts.push [y1, y2]
      cid := cid + 1
    let entryX := (next()).toInt!
    let entryY := (next()).toInt!
    let dirStr := next()
    let startDir : Nat :=
      if dirStr = "N" then 0
      else if dirStr = "E" then 1
      else if dirStr = "S" then 2
      else 3
    -- intersections
    for i in [0:hId.size] do
      let y := hY.get! i
      let x1 := hX1.get! i
      let x2 := hX2.get! i
      let xmin := min x1 x2
      let xmax := max x1 x2
      for j in [0:vId.size] do
        let x := vX.get! j
        let y1 := vY1.get! j
        let y2 := vY2.get! j
        let ymin := min y1 y2
        let ymax := max y1 y2
        if xmin ≤ x ∧ x ≤ xmax ∧ ymin ≤ y ∧ y ≤ ymax then
          hPts := hPts.modify i (fun l => x :: l)
          vPts := vPts.modify j (fun l => y :: l)
    -- add entry
    for i in [0:hId.size] do
      let y := hY.get! i
      if y = entryY then
        let x1 := hX1.get! i
        let x2 := hX2.get! i
        let xmin := min x1 x2
        let xmax := max x1 x2
        if xmin ≤ entryX ∧ entryX ≤ xmax then
          hPts := hPts.modify i (fun l => entryX :: l)
    for j in [0:vId.size] do
      let x := vX.get! j
      if x = entryX then
        let y1 := vY1.get! j
        let y2 := vY2.get! j
        let ymin := min y1 y2
        let ymax := max y1 y2
        if ymin ≤ entryY ∧ entryY ≤ ymax then
          vPts := vPts.modify j (fun l => entryY :: l)
    let ans := simulate hId hY hX1 hX2 hPts vId vX vY1 vY2 vPts entryX entryY startDir cid
    outs := outs.push (toString ans)
  for s in outs do
    IO.println s
