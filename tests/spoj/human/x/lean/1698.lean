/- Solution for SPOJ PLSEARCH - Polygonal Line Search
https://www.spoj.com/problems/PLSEARCH/
-/
import Std
open Std

abbrev Edge := (Int × Int)

-- rotate a vector by multiples of 90 degrees
def rotate (r : Nat) (e : Edge) : Edge :=
  let (x, y) := e
  match r % 4 with
  | 0 => (x, y)
  | 1 => (-y, x)
  | 2 => (-x, -y)
  | _ => (y, -x)

-- reverse traversal of edges
def revEdges (es : List Edge) : List Edge :=
  es.reverse.map (fun (dx, dy) => (-dx, -dy))

-- all rotations and reversed versions of the template
def variants (es : List Edge) : List (List Edge) :=
  let rot := [0,1,2,3].map (fun r => es.map (rotate r))
  rot ++ rot.map revEdges

-- parse a polygonal line, returning next index and edge list
partial def parsePoly (arr : Array String) (idx : Nat) : Nat × List Edge :=
  let m := (arr.get! idx).toNat!
  let mut verts : Array (Int × Int) := #[]
  for i in [0:m] do
    let x := (arr.get! (idx + 1 + 2*i)).toInt!
    let y := (arr.get! (idx + 1 + 2*i + 1)).toInt!
    verts := verts.push (x, y)
  let mut es : Array Edge := #[]
  for i in [0:m-1] do
    let (x1, y1) := verts.get! i
    let (x2, y2) := verts.get! (i+1)
    es := es.push (x2 - x1, y2 - y1)
  (idx + 1 + 2*m, es.toList)

-- process all datasets
partial def process (arr : Array String) (idx : Nat) (acc : Array String) : Array String :=
  let n := (arr.get! idx).toNat!
  if n = 0 then acc
  else
    let (idx1, tmpl) := parsePoly arr (idx + 1)
    let vars := variants tmpl
    let mut pos := idx1
    let mut res := acc
    for i in [0:n] do
      let (pos2, e) := parsePoly arr pos
      if e.length = tmpl.length && vars.any (fun v => v = e) then
        res := res.push (toString (i+1))
      pos := pos2
    res := res.push "+++++"
    process arr pos res

-- main entry point
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let arr := Array.ofList toks
  let outs := process arr 0 #[]
  IO.println (String.intercalate "\n" outs)
