-- https://www.spoj.com/problems/PALSEC
import Std
open Std

-- convert a String to an Array of Char
def strToArray (s : String) : Array Char :=
  s.data.toArray

-- check if array of chars is a palindrome
def isPal (a : Array Char) : Bool :=
  let rec loop (i j : Nat) : Bool :=
    if h : i < j then
      if a.get? i = a.get? j then
        loop (i+1) (j-1)
      else false
    else true
  if a.isEmpty then true else loop 0 (a.size - 1)

partial def solveCase (xs ys : Array (Array Char)) : IO Nat := do
  let n := xs.size
  let rxs := xs.map (·.reverse)
  let rys := ys.map (·.reverse)
  let memo ← IO.mkRef ({} : HashMap (Nat × Nat × Nat × Nat × Nat × Nat) Nat)
  partial def dfs (l i tL r j tR : Nat) : IO Nat := do
    let key := (l,i,tL,r,j,tR)
    let m ← memo.get
    match m.find? key with
    | some v => pure v
    | none =>
      let res ←
        if l > r then
          pure (if tL == 0 ∧ i == 0 ∧ tR == 0 ∧ j == 0 then 1 else 0)
        else if l == r then
          if tL == 0 ∧ tR == 0 then
            let mut cnt := 0
            if isPal (xs.get! l) then cnt := cnt + 1
            if isPal (ys.get! l) then cnt := cnt + 1
            pure cnt
          else
            let mut tL := tL
            let mut i := i
            let mut tR := tR
            let mut j := j
            if tL == 0 then
              tL := tR; i := 0
            if tR == 0 then
              tR := tL; j := 0
            if tL != tR then
              pure 0
            else
              let w := if tL == 1 then xs.get! l else ys.get! l
              let rec check (a b : Nat) : Bool :=
                if h : a < b then
                  match w.get? a, w.get? b with
                  | some c1, some c2 => if c1 == c2 then check (a+1) (b-1) else false
                  | _, _ => false
                else true
              pure (if check i (w.size - 1 - j) then 1 else 0)
        else if tL == 0 then
          let a ← dfs l 0 1 r j tR
          let b ← dfs l 0 2 r j tR
          pure (a + b)
        else if tR == 0 then
          let a ← dfs l i tL r 0 1
          let b ← dfs l i tL r 0 2
          pure (a + b)
        else
          let wL := if tL == 1 then xs.get! l else ys.get! l
          let wR := if tR == 1 then rxs.get! r else rys.get! r
          match wL.get? i, wR.get? j with
          | some c1, some c2 =>
              if c1 != c2 then pure 0 else
                let mut l := l
                let mut i := i + 1
                let mut tL := tL
                if h : i == wL.size then
                  l := l + 1; i := 0; tL := 0
                let mut r := r
                let mut j := j + 1
                let mut tR := tR
                if h : j == wR.size then
                  r := r - 1; j := 0; tR := 0
                dfs l i tL r j tR
          | _, _ => pure 0
      memo.modify fun m => m.insert key res
      pure res
  dfs 0 0 0 (n-1) 0 0

def main : IO Unit := do
  let t ← (← IO.getStdin).getLine
  let t := t.trim.toNat!
  let rec loop (k : Nat) : IO Unit :=
    if k == 0 then pure () else do
      let nLine ← (← IO.getStdin).getLine
      let n := nLine.trim.toNat!
      -- read X
      let mut i := 0
      let mut xsArr : Array (Array Char) := #[]
      while h : i < n do
        let s ← (← IO.getStdin).getLine
        xsArr := xsArr.push (strToArray s.trim)
        i := i + 1
      -- read Y
      let mut ysArr : Array (Array Char) := #[]
      let mut j := 0
      while h : j < n do
        let s ← (← IO.getStdin).getLine
        ysArr := ysArr.push (strToArray s.trim)
        j := j + 1
      let ans ← solveCase xsArr ysArr
      IO.println ans
      loop (k-1)
  loop t
