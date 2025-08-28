/- Solution for SPOJ DELCOMM - DEL Command
https://www.spoj.com/problems/DELCOMM/
-/
import Std
open Std

-- Split file name into base name and extension
def splitFile (s : String) : String × String :=
  match s.splitOn "." with
  | [n] => (n, "")
  | n :: e :: _ => (n, e)
  | _ => ("", "")

-- Build pattern (without dot) for list of names or extensions
-- using '?' for differing characters and optional '*' at the end
-- if lengths differ.
def buildPattern (xs : List String) : String :=
  let arrs := xs.map (·.toList)
  match arrs with
  | [] => ""
  | a :: rest =>
    let lmin := (rest.foldl (fun acc l => Nat.min acc l.length) a.length)
    let chars := (List.range lmin).map (fun i =>
      let c := a.get! i
      if (a :: rest).all (fun l => l.get! i = c) then c else '?')
    let base := String.mk chars
    let needStar := (a :: rest).any (fun l => l.length > lmin)
    base ++ (if needStar then "*" else "")

-- Match a string against a pattern built as above
def matchPrefix : List Char → List Char → Bool
  | [], _ => true
  | _ :: _, [] => false
  | pc :: pt, sc :: st =>
      if pc == '?' || pc == sc then matchPrefix pt st else false

def matchPart (pat s : String) : Bool :=
  let p := pat.toList
  let sl := s.toList
  if pat.endsWith "*" then
    let prefix := p.dropLast
    if prefix.length > sl.length then false
    else matchPrefix prefix (sl.take prefix.length)
  else
    if p.length != sl.length then false
    else matchPrefix p sl

-- Full filename match using name and extension patterns
def matches (namePat extPat file : String) : Bool :=
  let (n, e) := splitFile file
  if extPat = "" then
    e = "" && matchPart namePat n
  else
    matchPart namePat n && matchPart extPat e

-- Solve single dataset
def solveDataset (del keep : List String) : Option String :=
  let delParts := del.map splitFile
  let names := delParts.map Prod.fst
  let exts  := delParts.map Prod.snd
  let namePat := buildPattern names
  let extPat  := buildPattern exts
  let wildcard := namePat ++ (if extPat = "" then "" else "." ++ extPat)
  if keep.any (matches namePat extPat) then none else some wildcard

partial def readDataset (h : IO.FS.Stream) : IO (List String × List String) := do
  let rec skipBlank : IO String := do
    let line ← h.getLine
    let t := line.trim
    if t = "" then skipBlank else return t
  let mut line := (← skipBlank)
  let mut del : List String := []
  let mut keep : List String := []
  while true do
    if line != "" then
      let c := line.toList.head!
      if c = '-' then
        del := line.drop 1 :: del
      else
        keep := line.drop 1 :: keep
    let next := (← h.getLine).trim
    if next = "" then
      return (del.reverse, keep.reverse)
    else
      line := next
  return (del.reverse, keep.reverse)

partial def processDatasets (h : IO.FS.Stream) (m idx : Nat) : IO Unit := do
  if idx = m then return ()
  let (del, keep) ← readDataset h
  let res := match solveDataset del keep with
             | some w => "DEL " ++ w
             | none => "IMPOSSIBLE"
  IO.println res
  if idx + 1 < m then IO.println ""
  processDatasets h m (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let m := (← h.getLine).trim.toNat!
  processDatasets h m 0
