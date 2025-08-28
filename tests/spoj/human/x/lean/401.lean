/- Solution for SPOJ TRANSL - Translations
https://www.spoj.com/problems/TRANSL/
-/

import Std
open Std

structure Lang where
  words : Array String
  adj   : Array (Array Nat)

/-- read `n` phrases and build word list and adjacency matrix -/
def readLang (h : IO.FS.Stream) (n : Nat) : IO Lang := do
  let wordsRef ← IO.mkRef (#[] : Array String)
  let adjRef ← IO.mkRef (#[] : Array (Array Nat))
  let getIdx (w : String) : IO Nat := do
    let ws ← wordsRef.get
    match ws.findIdx? (· = w) with
    | some i => return i
    | none =>
        let i := ws.size
        wordsRef.set (ws.push w)
        adjRef.modify fun mat =>
          let mat := mat.map (fun row => row.push 0)
          mat.push (Array.replicate (i+1) 0)
        return i
  for _ in [0:n] do
    let line ← h.getLine
    let ws := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let ia ← getIdx ws[0]!
    let ib ← getIdx ws[1]!
    adjRef.modify fun mat =>
      let row := mat[ia]!
      let row := row.set! ib (row[ib]! + 1)
      mat.set! ia row
  return { words := ← wordsRef.get, adj := ← adjRef.get }

/-- compute out-degree and in-degree for each word -/
def degrees (lang : Lang) : Array Nat × Array Nat := Id.run do
  let m := lang.words.size
  let mut out := Array.replicate m 0
  let mut inn := Array.replicate m 0
  for i in [0:m] do
    let row := lang.adj[i]!
    for j in [0:m] do
      let c := row[j]!
      out := out.set! i (out[i]! + c)
      inn := inn.set! j (inn[j]! + c)
  return (out, inn)

/-- edge count between two indices -/
def edge (adj : Array (Array Nat)) (a b : Nat) : Nat :=
  adj[a]![b]!

partial def search (adj1 adj2 : Array (Array Nat))
    (cand : Array (List Nat))
    (assign : Array (Option Nat))
    (used : HashSet Nat) : Option (Array Nat) :=
  let m := assign.size
  if used.size == m then
    some <| Id.run do
      let mut res : Array Nat := Array.mkEmpty m
      for i in [0:m] do
        res := res.push (assign[i]! |> Option.get!)
      return res
  else
    Id.run do
      let mut bestIdx := 0
      let mut bestList : List Nat := []
      let mut bestCnt := m.succ
      for i in [0:m] do
        if assign[i]! == none then
          let avail := (cand[i]!).filter (fun j => ¬ used.contains j)
          let cnt := avail.length
          if cnt < bestCnt then
            bestIdx := i
            bestList := avail
            bestCnt := cnt
      if bestCnt == 0 then
        return none
      let mut ans : Option (Array Nat) := none
      for j in bestList do
        if ans.isSome then break
        let mut ok := edge adj1 bestIdx bestIdx == edge adj2 j j
        if ok then
          for k in [0:m] do
            match assign[k]! with
            | some v =>
                if edge adj1 bestIdx k != edge adj2 j v ||
                   edge adj1 k bestIdx != edge adj2 v j then
                  ok := false
            | none => ()
        if ok then
          let assign' := assign.set! bestIdx (some j)
          let used' := used.insert j
          let res := search adj1 adj2 cand assign' used'
          if res.isSome then
            ans := res
      return ans

partial def solve (l1 l2 : Lang) : Option (Array Nat) := Id.run do
  let m := l1.words.size
  if m != l2.words.size then
    return none
  let (out1, in1) := degrees l1
  let (out2, in2) := degrees l2
  let mut cand : Array (List Nat) := Array.mkEmpty m
  for i in [0:m] do
    let mut lst : List Nat := []
    for j in [0:m] do
      if out1[i]! == out2[j]! && in1[i]! == in2[j]! then
        lst := j :: lst
    cand := cand.push lst
  let assign := Array.replicate m (none : Option Nat)
  return search l1.adj l2.adj cand assign (HashSet.emptyWithCapacity 0)

partial def loop (h : IO.FS.Stream) (first : Bool) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let lang1 ← readLang h n
    let lang2 ← readLang h n
    let some mapping := solve lang1 lang2
      | throw <| IO.userError "no mapping"
    if !first then IO.println ""
    let idxs := (Array.range lang1.words.size).qsort
      (fun a b => lang1.words[a]! < lang1.words[b]!)
    for i in idxs do
      let j := mapping[i]!
      IO.println s!"{lang1.words[i]!}/{lang2.words[j]!}"
    loop h false

def main : IO Unit := do
  let h ← IO.getStdin
  loop h true
