/- Solution for SPOJ DELCOMM2 - DEL Command II
https://www.spoj.com/problems/DELCOMM2/
-/

import Std
open Std

/- Wildcard matching supporting '?' and '*'. -/
partial def wildMatchList : List Char → List Char → Bool
  | [], [] => true
  | [], _ => false
  | '*' :: ps, s =>
      if wildMatchList ps s then true else
        match s with
        | [] => false
        | _ :: ss => wildMatchList ('*' :: ps) ss
  | '?' :: ps, c :: ss => wildMatchList ps ss
  | '?' :: ps, [] => false
  | c :: ps, d :: ss => if c = d then wildMatchList ps ss else false
  | _ :: _, [] => false

def wildMatch (p s : String) : Bool :=
  wildMatchList p.data s.data

/-- Update the current best answer using a pattern. -/
def updateBest (pattern : String) (pos neg : Array String)
    (bestRef : IO.Ref Nat) (seenRef : IO.Ref (HashSet String)) : IO Unit := do
  let seen ← seenRef.get
  if seen.contains pattern then
    pure ()
  else
    seenRef.modify (fun h => h.insert pattern)
    -- ensure no negative matches
    let mut ok := true
    for n in neg do
      if wildMatch pattern n then
        ok := false
    if ok then
      let mut cnt := 0
      for p in pos do
        if wildMatch pattern p then
          cnt := cnt + 1
      let best ← bestRef.get
      if cnt > best then
        bestRef.set cnt

/-- Enumerate wildcard patterns derived from a string. -/
partial def enumerateFrom (s : String) (pos neg : Array String)
    (bestRef : IO.Ref Nat) (seenRef : IO.Ref (HashSet String)) : IO Unit :=
  let rec dfs (rest : List Char) (prevStar : Bool) (cur : String) : IO Unit := do
    match rest with
    | [] =>
        updateBest cur pos neg bestRef seenRef
        if ¬ prevStar then
          updateBest (cur.push '*') pos neg bestRef seenRef
    | c :: cs =>
        -- star continues to consume characters
        if prevStar then
          dfs cs true cur
        -- use literal character
        dfs cs false (cur.push c)
        -- replace with '?'
        dfs cs false (cur.push '?')
        -- insert '*' before current character
        if ¬ prevStar then
          dfs (c :: cs) true (cur.push '*')
  dfs s.data false ""

/-- Solve one dataset. -/
def solveDataset (pos neg : Array String) : IO Nat := do
  let bestRef ← IO.mkRef 0
  let seenRef ← IO.mkRef ({} : HashSet String)
  for s in pos do
    enumerateFrom s pos neg bestRef seenRef
  bestRef.get

/-- Parse the input into datasets. -/
private def parseDatasets (lines : List String) (m : Nat)
    : List (Array String × Array String) :=
  let rec skipEmpty : List String → List String
    | [] => []
    | h :: t => if h = "" then skipEmpty t else h :: t
  let rec readDataset : List String → Array String → Array String →
      (Array String × Array String × List String)
    | [], pos, neg => (pos, neg, [])
    | h :: t, pos, neg =>
        if h = "" then (pos, neg, t) else
          let parts := h.splitOn " "
          let name := parts.get! 0
          let sign := parts.get! 1
          if sign = "+" then
            readDataset t (pos.push name) neg
          else
            readDataset t pos (neg.push name)
  let rec loop : Nat → List String → List (Array String × Array String)
      → List (Array String × Array String)
    | 0, _, acc => acc.reverse
    | k+1, ls, acc =>
        let ls := skipEmpty ls
        let (pos, neg, rest) := readDataset ls #[] #[]
        loop k rest ((pos, neg) :: acc)
  loop m lines []

/-- Main entry point. -/
def main : IO Unit := do
  let content ← IO.readToEnd (← IO.getStdin)
  let mut lines := content.splitOn "\n" |>.map (·.trim)
  if h : lines ≠ [] then
    let m := (lines.get! 0).toNat!
    let rest := lines.drop 1
    let datasets := parseDatasets rest.toList m
    let mut outputs : Array String := #[]
    for (pos, neg) in datasets do
      let ans ← solveDataset pos neg
      outputs := outputs.push (toString ans)
    IO.println (String.intercalate "\n" outputs)
  else
    pure ()
