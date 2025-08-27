/- Solution for SPOJ BLOCKS - Arranging the Blocks
https://www.spoj.com/problems/BLOCKS/
-/

import Std
open Std

-- Parse list of natural numbers from input string
def parseNats (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filterMap (fun t =>
    if t.isEmpty then none else some t.toNat!)

-- read n x n matrix from list of tokens
partial def readMatrix (n : Nat) (xs : List Nat) : (Array (Array Nat) × List Nat) :=
  let rec build (i : Nat) (rest : List Nat) (acc : Array (Array Nat)) : (Array (Array Nat) × List Nat) :=
    if i = n then
      (acc, rest)
    else
      let rowList := rest.take n
      let rowArr : Array Nat := rowList.toArray
      build (i+1) (rest.drop n) (acc.push rowArr)
  build 0 xs #[]

-- check if rows of two matrices have the same multisets
partial def rowsSame (p q : Array (Array Nat)) (n : Nat) : Bool :=
  let rec loop (i : Nat) : Bool :=
    if i = n then true
    else
      let r1 := (p.get! i).qsort (· < ·)
      let r2 := (q.get! i).qsort (· < ·)
      if r1 == r2 then loop (i+1) else false
  loop 0

-- check if columns of two matrices have the same multisets
partial def colsSame (p q : Array (Array Nat)) (n : Nat) : Bool :=
  let rec loop (j : Nat) : Bool :=
    if j = n then true
    else
      let mut c1 : Array Nat := Array.mkEmpty n
      let mut c2 : Array Nat := Array.mkEmpty n
      for i in [0:n] do
        c1 := c1.push (p.get! i |>.get! j)
        c2 := c2.push (q.get! i |>.get! j)
      let c1 := c1.qsort (· < ·)
      let c2 := c2.qsort (· < ·)
      if c1 == c2 then loop (j+1) else false
  loop 0

-- solve single test case
partial def solve (n : Nat) (p q : Array (Array Nat)) : String :=
  let size := n * n + 1
  let mut diff : Array Int := Array.mkArray size 0
  let mut posP : Array (List (Nat × Nat)) := Array.mkArray size []
  let mut posQ : Array (List (Nat × Nat)) := Array.mkArray size []
  let mut same := true
  for i in [0:n] do
    for j in [0:n] do
      let a := p.get! i |>.get! j
      let b := q.get! i |>.get! j
      diff := diff.set! a (diff.get! a + 1)
      diff := diff.set! b (diff.get! b - 1)
      posP := posP.set! a ((i,j) :: posP.get! a)
      posQ := posQ.set! b ((i,j) :: posQ.get! b)
      if a ≠ b then same := false
  -- check counts
  let mut ok := true
  for x in diff do
    if x ≠ 0 then ok := false
  if !ok then
    "no"
  else if same then
    "0"
  else if rowsSame p q n || colsSame p q n then
    "1"
  else
    let mut rFirst := true
    let mut cFirst := true
    for v in [0:size] do
      match posP.get! v, posQ.get! v with
      | [ (r1,c1), (r2,c2) ], [ (R1,C1), (R2,C2) ] =>
          if r1 = r2 && C1 = C2 then rFirst := false
          if c1 = c2 && R1 = R2 then cFirst := false
      | _, _ => ()
    if rFirst || cFirst then
      "2"
    else
      "3"

partial def runCases : Nat → List Nat → List String → List String
| 0, _, acc => acc.reverse
| Nat.succ t, xs, acc =>
    match xs with
    | n :: rest =>
        let (p, rest1) := readMatrix n rest
        let (q, rest2) := readMatrix n rest1
        let ans := solve n p q
        runCases t rest2 (ans :: acc)
    | _ => acc.reverse

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let nums := parseNats input
  match nums with
  | [] => pure ()
  | t :: rest =>
      let lines := runCases t rest []
      IO.println (String.intercalate "\n" lines)
