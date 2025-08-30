/- Solution for SPOJ ZOO - Zoo
https://www.spoj.com/problems/ZOO/
-/

import Std
open Std

/-- Count happy children for a given 5-bit window. -/
def countHappy (kids : List (Nat × Nat)) (w : Nat) : Nat :=
  kids.foldl (fun acc (p : Nat × Nat) =>
    let f := p.fst
    let l := p.snd
    if (w &&& f) ≠ 0 || (w &&& l) ≠ l then acc + 1 else acc) 0

/-- Solve a single test case. -/
def solveCase (n : Nat) (kids : Array (List (Nat × Nat))) : Nat :=
  Id.run do
    let mut best : Int := 0
    for init in [0:16] do
      let mut dp := Array.mkArray 16 (-1 : Int)
      dp := dp.set! init 0
      for i in [0:n] do
        let ks := kids[i]!
        let mut ndp := Array.mkArray 16 (-1 : Int)
        for s in [0:16] do
          let v := dp[s]!
          if v >= 0 then
            for b in [0:2] do
              let win := s ||| (b <<< 4)
              let happy := countHappy ks win
              let ns := (s >>> 1) ||| (b <<< 3)
              let nv := v + (Int.ofNat happy)
              let cur := ndp[ns]!
              if nv > cur then
                ndp := ndp.set! ns nv
        dp := ndp
      let final := dp[init]!
      if final > best then
        best := final
    return Int.toNat best

/-- Parse input tokens recursively. -/
partial def solve (toks : Array String) : Array String := Id.run do
  let t := toks[0]!.toNat!
  let mut idx := 1
  let mut res : Array String := #[]
  for _ in [0:t] do
    let n := toks[idx]!.toNat!; idx := idx + 1
    let c := toks[idx]!.toNat!; idx := idx + 1
    let mut kids : Array (List (Nat × Nat)) := Array.mkArray n []
    for _ in [0:c] do
      let e := toks[idx]!.toNat!; idx := idx + 1
      let fcnt := toks[idx]!.toNat!; idx := idx + 1
      let lcnt := toks[idx]!.toNat!; idx := idx + 1
      let mut fmask := 0
      for _ in [0:fcnt] do
        let x := toks[idx]!.toNat!
        let rel := (x + n - e) % n
        if rel < 5 then
          fmask := fmask ||| (1 <<< rel)
        idx := idx + 1
      let mut lmask := 0
      for _ in [0:lcnt] do
        let y := toks[idx]!.toNat!
        let rel := (y + n - e) % n
        if rel < 5 then
          lmask := lmask ||| (1 <<< rel)
        idx := idx + 1
      let pos := e - 1
      let lst := kids[pos]!
      kids := kids.set! pos ((fmask, lmask) :: lst)
    res := res.push (toString (solveCase n kids))
  return res

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let out := solve toks
  for line in out do
    IO.println line
