/- Solution for SPOJ CTGAME - City Game
https://www.spoj.com/problems/CTGAME/
-/

import Std
open Std

/-- compute largest rectangle area in histogram `hs` -/
private def largestRect (hs : Array Nat) : Nat :=
  let n := hs.size
  let rec loop (i : Nat) (stack : List Nat) (best : Nat) : Nat :=
    if h : i < n then
      let hi := hs.get! i
      let rec pop (st : List Nat) (best : Nat) : List Nat × Nat :=
        match st with
        | [] => ([], best)
        | top :: rest =>
          if hs.get! top >= hi then
            let width :=
              if rest = [] then i else i - rest.head! - 1
            let area := hs.get! top * width
            pop rest (Nat.max best area)
          else
            (st, best)
      let (stack, best) := pop stack best
      loop (i+1) (i :: stack) best
    else
      let rec finish (st : List Nat) (best : Nat) : Nat :=
        match st with
        | [] => best
        | top :: rest =>
            let width :=
              if rest = [] then n else n - rest.head! - 1
            let area := hs.get! top * width
            finish rest (Nat.max best area)
      finish stack best
  loop 0 [] 0

/-- update histogram heights with a row of tokens "F" or "R" -/
private def updateHeights (hs : Array Nat) (row : List String) : Array Nat :=
  let rec go (i : Nat) (hs : Array Nat) (tokens : List String) : Array Nat :=
    match tokens with
    | [] => hs
    | t :: ts =>
        let h := hs.get! i
        let h' := if t = "F" then h + 1 else 0
        let hs := hs.set! i h'
        go (i+1) hs ts
  go 0 hs row

/-- solve one test case reading from stream `h` -/
private def solveCase (h : IO.FS.Stream) : IO Nat := do
  let header ← readNonEmptyLine h
  let nums := header.split (fun c => c = ' ') |>.filter (· ≠ "")
  let m := nums.get! 0 |>.toNat!
  let n := nums.get! 1 |>.toNat!
  let mut heights : Array Nat := Array.mkArray n 0
  let mut best : Nat := 0
  for _ in [0:m] do
    let line ← readNonEmptyLine h
    let tokens := line.split (fun c => c = ' ') |>.filter (· ≠ "")
    heights := updateHeights heights tokens
    let area := largestRect heights
    if area > best then
      best := area
  pure best

partial def readNonEmptyLine (h : IO.FS.Stream) : IO String := do
  let line ← h.getLine
  let line := line.trim
  if line = "" then
    readNonEmptyLine h
  else
    pure line

/-- main: read input, output profit for each test case -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for i in [0:t] do
    let area ← solveCase h
    IO.println (area * 3)
