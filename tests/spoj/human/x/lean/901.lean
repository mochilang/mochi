/- Solution for SPOJ Index Generation
https://www.spoj.com/problems/INDEXGEN/
-/

import Std
open Std

structure PEntry where
  pages : Std.HashSet Nat := {}
  subs  : Std.HashMap String (Std.HashSet Nat) := {}

def addPrimary (m : Std.HashMap String PEntry) (p : String) (pg : Nat) : Std.HashMap String PEntry :=
  let e := m.findD p {}
  let e := {e with pages := e.pages.insert pg}
  m.insert p e

def addSecondary (m : Std.HashMap String PEntry) (p s : String) (pg : Nat) : Std.HashMap String PEntry :=
  let e := m.findD p {}
  let sp := e.subs.findD s {}
  let e := {e with subs := e.subs.insert s (sp.insert pg)}
  m.insert p e

partial def parseMarker (cs : List Char) : String × List Char :=
  let rec loop : List Char → Option Char → List Char → String × List Char
    | [], _, acc => (String.mk (acc.reverse), [])
    | c :: rest, prev, acc =>
        if c = '}' then
          (String.mk (acc.reverse), rest)
        else
          let c' := if c = '\n' then ' ' else c
          let nxt := match rest with
            | [] => none
            | d :: _ => some (if d = '\n' then ' ' else d)
          let skip := c' = ' ' ∧ ((prev = none) ∨ prev = some '%' ∨ prev = some '$' ∨ nxt = some '%' ∨ nxt = some '$' ∨ nxt = some '}')
          if skip then
            loop rest prev acc
          else
            loop rest (some c') (c' :: acc)
  loop cs none []

partial def findIdx? (cs : List Char) (ch : Char) : Option Nat :=
  let rec go : List Char → Nat → Option Nat
    | [], _ => none
    | c :: rest, i => if c = ch then some i else go rest (i+1)
  go cs 0

partial def substring (cs : List Char) (start stop : Nat) : String :=
  String.mk <| (cs.drop start).take (stop - start)

partial def substringToEnd (cs : List Char) (start : Nat) : String :=
  String.mk <| cs.drop start

partial def processMarker (m : Std.HashMap String PEntry) (content : String) (pg : Nat) : Std.HashMap String PEntry :=
  let cs := content.data.toList
  let idxPct := findIdx? cs '%'
  let idxDol := findIdx? cs '$'
  match idxPct, idxDol with
  | none, none => addPrimary m content pg
  | some p, none => addPrimary m (substringToEnd cs (p+1)) pg
  | none, some d =>
      let prim := substring cs 0 d
      let sec := substringToEnd cs (d+1)
      addSecondary m prim sec pg
  | some p, some d =>
      let prim := substring cs (p+1) d
      let sec := substringToEnd cs (d+1)
      addSecondary m prim sec pg

partial def process (cs : List Char) (pg : Nat) (m : Std.HashMap String PEntry) : Std.HashMap String PEntry :=
  match cs with
  | [] => m
  | c :: rest =>
      if c = '&' then
        process rest (pg+1) m
      else if c = '{' then
        let (content, rest') := parseMarker rest
        let m := processMarker m content pg
        process rest' pg m
      else
        process rest pg m

partial def formatPages (s : Std.HashSet Nat) : String :=
  let lst := s.toList.qsort (fun x y => x < y)
  String.intercalate ", " (lst.map toString)

partial def buildOutput (m : Std.HashMap String PEntry) : List String :=
  let entries := m.toList.qsort (fun a b => a.fst.toLower < b.fst.toLower)
  entries.foldl (fun acc (name, pe) =>
    let line :=
      let pages := formatPages pe.pages
      if pages = "" then name else name ++ ", " ++ pages
    let subs := pe.subs.toList.qsort (fun a b => a.fst.toLower < b.fst.toLower)
    let subsLines := subs.map (fun (s, set) =>
      let pg := formatPages set
      "+ " ++ s ++ ", " ++ pg)
    acc ++ (line :: subsLines)
  ) []

partial def processDoc (lines : List String) : List String :=
  let text := String.intercalate "\n" lines
  let chars := text.data.toList
  let m := process chars 1 {}
  buildOutput m

partial def loopDocs (docs : List (List String)) (n : Nat) : List String :=
  match docs with
  | [] => []
  | d :: ds =>
      let header := s!"DOCUMENT {n}"
      let body := processDoc d
      header :: body ++ loopDocs ds (n+1)

partial def splitDocs (lines : List String) : List (List String) :=
  let rec go (ls acc cur) :=
    match ls with
    | [] => (cur.reverse) :: acc
    | l :: ls' =>
        if l = "*" then
          go ls' ((cur.reverse) :: acc) []
        else
          go ls' acc (l :: cur)
  (go lines [] []).reverse

partial def readInput : IO (List String) := do
  let rec collect (acc : List String) := do
    let l ← IO.getLine
    if l.trim = "**" then
      pure (acc.reverse)
    else
      collect (l :: acc)
  collect []

partial def main : IO Unit := do
  let lines ← readInput
  let docs := splitDocs lines
  let output := loopDocs docs 1
  for line in output do
    IO.println line

#eval main
