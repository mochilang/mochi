/- Solution for SPOJ ARITH - A Simple Calculator
https://www.spoj.com/problems/ARITH/
-/

import Std
open Std

def repeatChar (c : Char) (n : Nat) : String :=
  String.mk (List.replicate n c)

-- pad string on the left with spaces to reach width w
def padLeft (s : String) (w : Nat) : String :=
  repeatChar ' ' (w - s.length) ++ s

-- split expression of form "number op number"
def splitExpr (s : String) : String × Char × String :=
  let rec loop (i : Nat) (cs : List Char) : String × Char × String :=
    match cs with
    | [] => ("", '+', "")
    | c :: cs =>
      if c = '+' ∨ c = '-' ∨ c = '*' then
        let left := s.take i
        let right := s.drop (i+1)
        (left, c, right)
      else
        loop (i+1) cs
  loop 0 s.toList

-- process a single expression
def formatExpr (aStr : String) (op : Char) (bStr : String) : List String :=
  let aNat := aStr.toNat!
  let bNat := bStr.toNat!
  let resStr :=
    match op with
    | '+' => toString (aNat + bNat)
    | '-' => toString (aNat - bNat)
    | '*' => toString (aNat * bNat)
    | _   => ""
  let opb := String.mk [op] ++ bStr
  match op with
  | '+' | '-' =>
      let width := Nat.max (Nat.max aStr.length opb.length) resStr.length
      let line1 := padLeft aStr width
      let line2 := padLeft opb width
      let dashLen := Nat.max opb.length resStr.length
      let line3 := padLeft (repeatChar '-' dashLen) width
      let line4 := padLeft resStr width
      [line1, line2, line3, line4]
  | '*' =>
      let digits := bStr.toList.reverse
      let rec parts (ds : List Char) (idx : Nat) (acc : List (String × Nat)) (mx : Nat) : List (String × Nat) × Nat :=
        match ds with
        | [] => (acc.reverse, mx)
        | c :: cs =>
          let d := c.toNat - '0'.toNat
          let pStr := toString (aNat * d)
          let mx := Nat.max mx (pStr.length + idx)
          parts cs (idx + 1) ((pStr, idx) :: acc) mx
      let (partials, mxPart) := parts digits 0 [] 0
      let width := Nat.max (Nat.max (Nat.max aStr.length opb.length) resStr.length) mxPart
      if bStr.length = 1 then
        let dashLen := Nat.max opb.length resStr.length
        let line1 := padLeft aStr width
        let line2 := padLeft opb width
        let line3 := padLeft (repeatChar '-' dashLen) width
        let line4 := padLeft resStr width
        [line1, line2, line3, line4]
      else
        let firstPartLen := partials.head!.fst.length
        let dash1Len := Nat.max opb.length firstPartLen
        let line1 := padLeft aStr width
        let line2 := padLeft opb width
        let line3 := padLeft (repeatChar '-' dash1Len) width
        let partLines := partials.map (fun (p, sh) => padLeft p (width - sh))
        let dash2Len := Nat.max resStr.length mxPart
        let line4 := padLeft (repeatChar '-' dash2Len) width
        let line5 := padLeft resStr width
        [line1, line2, line3] ++ partLines ++ [line4, line5]
  | _ => []

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := tLine.trim.toNat!
  for _ in [0:t] do
    let line ← h.getLine
    let (a, op, b) := splitExpr (line.trim)
    let lines := formatExpr a op b
    for l in lines do
      IO.println l
    IO.println ""
