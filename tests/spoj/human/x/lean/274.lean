/- Solution for SPOJ WMELON - Johnny and the Watermelon Plantation
https://www.spoj.com/problems/WMELON/
-/

import Std
open Std

structure Plant where
  x : Nat
  y : Nat
  f : Nat

/-- compute minimal area rectangle covering at least k fruits -/
def minArea (k : Nat) (plants : Array Plant) : Nat :=
  Id.run do
    let maxC := 1000
    let mut xUsed : Array Bool := Array.replicate (maxC+1) false
    let mut yUsed : Array Bool := Array.replicate (maxC+1) false
    for p in plants do
      xUsed := xUsed.set! p.x true
      yUsed := yUsed.set! p.y true
    let mut xArr : Array Nat := #[]
    let mut xIdx : Array Nat := Array.replicate (maxC+1) 0
    let mut xCnt := 0
    for x in [0:maxC+1] do
      if xUsed[x]! then
        xArr := xArr.push x
        xIdx := xIdx.set! x xCnt
        xCnt := xCnt + 1
    let mut yArr : Array Nat := #[]
    let mut yIdx : Array Nat := Array.replicate (maxC+1) 0
    let mut yCnt := 0
    for y in [0:maxC+1] do
      if yUsed[y]! then
        yArr := yArr.push y
        yIdx := yIdx.set! y yCnt
        yCnt := yCnt + 1
    let m := xArr.size
    let l := yArr.size
    let mut cols : Array (Array (Nat × Nat)) := Array.replicate m #[]
    for p in plants do
      let xid := xIdx[p.x]!
      let yid := yIdx[p.y]!
      let arr := cols[xid]!
      cols := cols.set! xid (arr.push (yid, p.f))
    let mut best : Nat := 1000000000
    for x1 in [0:m] do
      let mut sums : Array Nat := Array.replicate l 0
      for x2 in [x1:m] do
        for (yi, f) in cols[x2]! do
          sums := sums.set! yi (sums[yi]! + f)
        let width := xArr[x2]! - xArr[x1]!
        let mut total := 0
        let mut lo := 0
        for hi in [0:l] do
          total := total + sums[hi]!
          while total >= k && lo ≤ hi do
            let height := yArr[hi]! - yArr[lo]!
            let area := width * height
            if area < best then
              best := area
            total := total - sums[lo]!
            lo := lo + 1
    return best

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := toks[idx]!.toNat!
    let k := toks[idx+1]!.toNat!
    let rec read (cnt : Nat) (j : Nat) (accP : Array Plant) : Array Plant × Nat :=
      if cnt = 0 then (accP, j) else
        let x := toks[j]!.toNat!
        let y := toks[j+1]!.toNat!
        let f := toks[j+2]!.toNat!
        read (cnt-1) (j+3) (accP.push {x:=x, y:=y, f:=f})
    let (plants, j) := read n (idx+2) #[]
    let ans := minArea k plants
    solveAll toks j (t-1) (s!"{ans}" :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
