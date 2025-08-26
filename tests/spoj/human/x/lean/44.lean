/- Solution for SPOJ SCYPHER - Substitution Cipher
https://www.spoj.com/problems/SCYPHER/
-/

import Std
open Std

/-- find first differing characters between two lists --/
partial def firstDiff : List Char -> List Char -> Option (Char × Char)
  | [], [] => none
  | [], _  => none
  | _, []  => none
  | c1 :: t1, c2 :: t2 =>
      if c1 = c2 then firstDiff t1 t2 else some (c1, c2)

/-- depth-first search from `start` returning visited nodes and whether start is reachable --/
partial def dfsReach (adj : Array (List Nat)) (start n : Nat)
    : Array Bool × Bool :=
  let rec loop : List Nat -> Array Bool -> Bool -> Array Bool × Bool
    | [], vis, found => (vis, found)
    | x :: xs, vis, found =>
        if x = start then
          loop xs vis true
        else if vis.get! x then
          loop xs vis found
        else
          let vis := vis.set! x true
          loop ((adj.get! x) ++ xs) vis found
  loop (adj.get! start) (Array.mkArray n false) false

/-- solve one case --/
partial def solveCase (A K : Nat) (words : Array String) (msg : String)
    : Option String :=
  let base := 'a'.toNat
  let mut adj : Array (List Nat) := Array.mkArray A []
  let mut rev : Array (List Nat) := Array.mkArray A []
  let mut invalid := false
  for i in [0:K-1] do
    let w1 := words.get! i |>.data
    let w2 := words.get! (i+1) |>.data
    let diff := firstDiff w1 w2
    match diff with
    | some (c1, c2) =>
        let u := c1.toNat - base
        let v := c2.toNat - base
        let row := adj.get! u
        if row.contains v then
          pure ()
        else
          adj := adj.set! u (v :: row)
          rev := rev.set! v (u :: rev.get! v)
    | none =>
        if (words.get! i).length >= (words.get! (i+1)).length then
          invalid := true
  if invalid then
    none
  else
    let mut succCnt := Array.mkArray A 0
    let mut predCnt := Array.mkArray A 0
    let mut hasCycle := false
    for i in [0:A] do
      let (vis, cyc) := dfsReach adj i A
      if cyc then hasCycle := true
      let mut c := 0
      for j in [0:A] do
        if vis.get! j then c := c + 1
      succCnt := succCnt.set! i c
      let (visP, _) := dfsReach rev i A
      let mut cp := 0
      for j in [0:A] do
        if visP.get! j then cp := cp + 1
      predCnt := predCnt.set! i cp
    if hasCycle then none else
      let mut lower := Array.mkArray A 0
      let mut upper := Array.mkArray A 0
      for i in [0:A] do
        lower := lower.set! i (predCnt.get! i)
        upper := upper.set! i (A - 1 - succCnt.get! i)
      let mut used : Array Bool := Array.mkArray A false
      for c in msg.data do
        let cn := c.toNat
        if cn >= base && cn < base + A then
          used := used.set! (cn - base) true
      let mut ok := true
      for i in [0:A] do
        if lower.get! i > upper.get! i then ok := false
      for i in [0:A] do
        if used.get! i && lower.get! i ≠ upper.get! i then ok := false
      if !ok then none else
        let mut map := Array.mkArray A 0
        for i in [0:A] do
          map := map.set! i (lower.get! i)
        let mut outChars : List Char := []
        for c in msg.data do
          let cn := c.toNat
          if cn >= base && cn < base + A then
            let idx := cn - base
            let pc := Char.ofNat (base + map.get! idx)
            outChars := pc :: outChars
          else
            outChars := c :: outChars
        some (String.mk outChars.reverse)

/-- process all cases --/
partial def loopCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line := (← h.getLine)
    let parts := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let A := parts.get! 0 |>.toNat!
    let K := parts.get! 1 |>.toNat!
    let mut words : Array String := Array.mkArray K ""
    for i in [0:K] do
      words := words.set! i (← h.getLine).trim
    let msg := (← h.getLine)
    match solveCase A K words msg with
    | some s => IO.println s
    | none   => IO.println "Message cannot be decrypted."
    loopCases h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loopCases h t
