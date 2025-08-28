/- Solution for SPOJ DOUBLEVI - Double Vision
https://www.spoj.com/problems/DOUBLEVI/
-/

import Std
open Std

partial def findSingle (arr : Array Bool) (counts : Array Nat) (p total : Nat) : Option Nat :=
  if p == total then none
  else if arr[p]! && counts[p]! == 1 then some p
  else findSingle arr counts (p+1) total

partial def checkPair (arrs : Array (Array Bool)) (s n p q t : Nat) : Bool :=
  if t == n then true
  else if t == s then checkPair arrs s n p q (t+1)
  else
    let a := arrs[t]!
    if a[p]! && a[q]! then false
    else checkPair arrs s n p q (t+1)

partial def findPair (arrs : Array (Array Bool)) (arr : Array Bool) (s n total p : Nat) : Option (Nat × Nat) :=
  if p == total then none
  else if !arr[p]! then findPair arrs arr s n total (p+1)
  else
    let rec findQ (q : Nat) : Option (Nat × Nat) :=
      if q == total then findPair arrs arr s n total (p+1)
      else if q ≤ p || !arr[q]! then findQ (q+1)
      else if checkPair arrs s n p q 0 then some (p,q)
      else findQ (q+1)
    findQ (p+1)

partial def process (n r c : Nat) (symbols : Array (Array (Array Char))) : Option (Array (Array (Array Char))) :=
  Id.run do
    let total := r * c
    let mut arrs : Array (Array Bool) := Array.replicate n (Array.replicate total false)
    let mut counts : Array Nat := Array.replicate total 0
    for s in [0:n] do
      for i in [0:r] do
        let row := (symbols[s]!)[i]!
        for j in [0:c] do
          if row[j]! == 'o' then
            let p := i * c + j
            arrs := arrs.modify s (fun a => a.set! p true)
            counts := counts.set! p (counts[p]! + 1)
    let rec loop (idx : Nat) (symbs : Array (Array (Array Char))) : Id (Option (Array (Array (Array Char)))) := do
      if idx == n then
        return some symbs
      else
        let arr := arrs[idx]!
        match findSingle arr counts 0 total with
        | some p =>
            let r1 := p / c
            let c1 := p % c
            let rows := symbs[idx]!
            let row := rows[r1]!
            let row' := row.set! c1 '#'
            let rows := rows.set! r1 row'
            let symbs := symbs.set! idx rows
            loop (idx+1) symbs
        | none =>
            match findPair arrs arr idx n total 0 with
            | some (p,q) =>
                let r1 := p / c; let c1 := p % c
                let r2 := q / c; let c2 := q % c
                let rows := symbs[idx]!
                let row1 := rows[r1]!
                let row1' := row1.set! c1 '#'
                let rows := rows.set! r1 row1'
                let row2 := rows[r2]!
                let row2' := row2.set! c2 '#'
                let rows := rows.set! r2 row2'
                let symbs := symbs.set! idx rows
                loop (idx+1) symbs
            | none =>
                return none
    loop 0 symbols

partial def parseSymbols (tokens : Array String) (start n r c : Nat) : (Array (Array (Array Char)) × Nat) :=
  Id.run do
    let mut idx := start
    let mut symbols : Array (Array (Array Char)) := Array.replicate n (Array.replicate r (Array.replicate c '.'))
    for i in [0:r] do
      for s in [0:n] do
        let str := tokens[idx]!
        idx := idx + 1
        symbols := symbols.modify s (fun rows => rows.set! i (str.toList.toArray))
    return (symbols, idx)

partial def loop (tokens : Array String) (i caseNo : Nat) (acc : Array String) : Array String :=
  let n := tokens[i]!.toNat!
  let r := tokens[i+1]!.toNat!
  let c := tokens[i+2]!.toNat!
  if n == 0 && r == 0 && c == 0 then acc
  else
    let (symbols, nextIdx) := parseSymbols tokens (i+3) n r c
    let acc := acc.push s!"Test {caseNo}"
    match process n r c symbols with
    | none =>
      let acc := acc.push "impossible"
      loop tokens nextIdx (caseNo+1) acc
    | some symbs =>
      let acc := Id.run do
        let mut acc := acc
        for irow in [0:r] do
          let mut parts : Array String := #[]
          for s in [0:n] do
            let rowArr := (symbs[s]!)[irow]!
            parts := parts.push (String.mk (rowArr.toList))
          acc := acc.push (String.intercalate " " parts.toList)
        return acc
      loop tokens nextIdx (caseNo+1) acc

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokensList := data.split (fun ch => ch = ' ' || ch = '\n' || ch = '\t' || ch = '\r')
                     |>.filter (fun s => s ≠ "")
  let tokens := tokensList.toArray
  let outs := loop tokens 0 1 #[]
  IO.println (String.intercalate "\n" outs.toList)
