-- Solution for SPOJ PT07H - Search in XML
-- https://www.spoj.com/problems/PT07H
import Std
open Std

structure Node where
  name : String
  children : List Node
  id : Nat
  deriving Inhabited

partial def readName (cs : List Char) (acc : List Char := []) : String × List Char :=
  match cs with
  | [] => (String.mk acc.reverse, [])
  | c :: rest =>
      if c = '>' then (String.mk acc.reverse, rest)
      else readName rest (c :: acc)

partial def parseNode : List Char → Nat → (Node × List Char × Nat)
  | '<' :: rest, nextId =>
      let (nm, rest) := readName rest
      let id := nextId
      let nextId := nextId + 1
      let rec parseChildren (cs : List Char) (children : List Node) (nid : Nat)
            : (List Node × List Char × Nat) :=
        match cs with
        | '<' :: '/' :: r =>
            let (_cn, r) := readName r
            (children.reverse, r, nid)
        | '<' :: _ =>
            let (child, r, nid') := parseNode cs nid
            parseChildren r (child :: children) nid'
        | _ => (children.reverse, cs, nid)
      let (ch, rest, nextId) := parseChildren rest [] nextId
      ({name := nm, children := ch, id := id}, rest, nextId)
  | cs, nid => ({name := "", children := [], id := nid}, cs, nid)

partial def matchPattern (t p : Node) : Bool :=
  if t.name != p.name then false
  else
    let rec check (pcs : List Node) : Bool :=
      match pcs with
      | [] => true
      | pc :: ps =>
          match t.children.find? (fun c => c.name = pc.name) with
          | none => false
          | some tc => if matchPattern tc pc then check ps else false
    check p.children

partial def collectMatches (t p : Node) : List Nat :=
  let rest := t.children.foldl (fun acc c => acc ++ collectMatches c p) []
  if matchPattern t p then t.id :: rest else rest

def parseInput (s : String) : (Node × Node) :=
  let cs := s.data.filter (fun c => !c.isWhitespace)
  let (text, rest, _) := parseNode cs 1
  let (pat, _, _) := parseNode rest 0
  (text, pat)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let (text, pat) := parseInput data
  let ms := collectMatches text pat
  IO.println ms.length
  for id in ms do
    IO.println id
