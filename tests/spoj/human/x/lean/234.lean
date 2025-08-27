/- Solution for SPOJ HOLIDAY1 - Getting Rid of the Holidays (Act I)
https://www.spoj.com/problems/HOLIDAY1/
-/

import Std
open Std

partial def search (m : Nat) (adj : Array (Array Bool))
    (clique : List Nat) (cand : List Nat) : Option (List Nat) :=
  if clique.length == m then
    some clique
  else if clique.length + cand.length < m then
    none
  else
    match cand with
    | [] => none
    | v :: rest =>
        let row := adj.get! v
        let newCand := rest.filter (fun w => row.get! w)
        match search m adj (v :: clique) newCand with
        | some res => some res
        | none => search m adj clique rest

def findClique (m : Nat) (adj : Array (Array Bool)) (n : Nat) : Option (List Nat) :=
  search m adj [] (List.range n)

partial def solveCase (n k : Nat) (ts : Array Nat) : List Nat :=
  let m := n - k
  let gcdMat := ts.map (fun a => ts.map (fun b => Nat.gcd a b))
  let mut gs : List Nat := []
  for i in [0:n] do
    gs := (ts.get! i) :: gs
    for j in [i+1:n] do
      gs := Nat.gcd (ts.get! i) (ts.get! j) :: gs
  let gs := (gs.eraseDups).qsort (fun a b => b < a)
  let rec tryGs : List Nat -> Option (List Nat)
    | [] => none
    | g :: rest =>
        let adj := gcdMat.map (fun row => row.map (fun x => x >= g))
        match findClique m adj n with
        | some cl => some cl
        | none => tryGs rest
  let keep := (tryGs gs).getD []
  let rem := (List.range n).filter (fun i => !(keep.contains i))
  rem.map (· + 1)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let ws := line.trim.split (· = ' ')
    let n := ws.get! 0 |>.toNat!
    let k := ws.get! 1 |>.toNat!
    let line2 ← h.getLine
    let ts := line2.trim.split (· = ' ') |>.map String.toNat! |> Array.ofList
    let rem := solveCase n k ts
    let out := String.intercalate " " (rem.map toString)
    IO.println out
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
