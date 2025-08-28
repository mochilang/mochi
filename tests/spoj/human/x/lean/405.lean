/- Solution for SPOJ TCUTTER - Tin Cutter
   https://www.spoj.com/problems/TCUTTER/
-/

import Std

open Std

structure Point where
  x : Int
  y : Int
  deriving BEq, Hashable, Inhabited

structure Segment where
  a : Point
  b : Point
  deriving Inhabited

def isVertical (s : Segment) : Bool := s.a.x == s.b.x

def isHorizontal (s : Segment) : Bool := s.a.y == s.b.y

def intersect? (s1 s2 : Segment) : Option Point :=
  if isVertical s1 && isHorizontal s2 then
    let xv := s1.a.x
    let yh := s2.a.y
    if (min s1.a.y s1.b.y) ≤ yh ∧ yh ≤ (max s1.a.y s1.b.y) ∧
       (min s2.a.x s2.b.x) ≤ xv ∧ xv ≤ (max s2.a.x s2.b.x) then
      some ⟨xv, yh⟩
    else none
  else if isVertical s2 && isHorizontal s1 then
    let xv := s2.a.x
    let yh := s1.a.y
    if (min s2.a.y s2.b.y) ≤ yh ∧ yh ≤ (max s2.a.y s2.b.y) ∧
       (min s1.a.x s1.b.x) ≤ xv ∧ xv ≤ (max s1.a.x s1.b.x) then
      some ⟨xv, yh⟩
    else none
  else
    none

def sortPoints (pts : List Point) (vertical : Bool) : List Point :=
  let arr := pts.toArray
  let arr := if vertical then arr.qsort (fun a b => a.y < b.y)
             else arr.qsort (fun a b => a.x < b.x)
  arr.toList

def dedupSorted : List Point → List Point
  | [] => []
  | h :: t =>
    let rec loop (prev : Point) (rest : List Point) (acc : List Point) : List Point :=
      match rest with
      | [] => (List.reverse (prev :: acc))
      | x :: xs =>
        if x == prev then
          loop prev xs acc
        else
          loop x xs (prev :: acc)
    loop h t []

def insertAdj (adj : HashMap Point (List Point)) (p q : Point) : HashMap Point (List Point) :=
  let adj := adj.insert p (q :: adj.getD p [])
  adj.insert q (p :: adj.getD q [])

partial def addEdges (pts : List Point) (adj : HashMap Point (List Point)) (e : Nat)
  : HashMap Point (List Point) × Nat :=
  match pts with
  | p :: q :: rest =>
      let adj := insertAdj adj p q
      addEdges (q :: rest) adj (e + 1)
  | _ => (adj, e)

partial def dfs (adj : HashMap Point (List Point)) (start : Point)
  (visited : HashSet Point) : HashSet Point :=
  let rec loop (stack : List Point) (vis : HashSet Point) : HashSet Point :=
    match stack with
    | [] => vis
    | p :: rest =>
        if vis.contains p then
          loop rest vis
        else
          let vis := vis.insert p
          let neigh := adj.getD p []
          loop (neigh ++ rest) vis
  loop [start] visited

partial def countComponents (adj : HashMap Point (List Point)) : Nat :=
  let keys := adj.toList.map (fun pr => pr.fst)
  let rec loop (ks : List Point) (vis : HashSet Point) (c : Nat) : Nat :=
    match ks with
    | [] => c
    | p :: rest =>
        if vis.contains p then
          loop rest vis c
        else
          let vis := dfs adj p vis
          loop rest vis (c + 1)
  loop keys (HashSet.empty) 0

def countHoles (segsList : List Segment) : Nat :=
  Id.run do
    let segs := segsList.toArray
    let m := segs.size
    let mut ptsOn : Array (List Point) := Array.mkArray m []
    for i in [0:m] do
      let s := segs.get! i
      ptsOn := ptsOn.set! i [s.a, s.b]
    for i in [0:m] do
      for j in [i+1:m] do
        let s1 := segs.get! i
        let s2 := segs.get! j
        match intersect? s1 s2 with
        | some p =>
            ptsOn := ptsOn.modify i (fun lst => p :: lst)
            ptsOn := ptsOn.modify j (fun lst => p :: lst)
        | none => pure ()
    let mut adj : HashMap Point (List Point) := {}
    let mut edges : Nat := 0
    for i in [0:m] do
      let s := segs.get! i
      let pts := ptsOn.get! i
      let pts := dedupSorted (sortPoints pts (isVertical s))
      let (adj', e') := addEdges pts adj edges
      adj := adj'
      edges := e'
    let vertices := adj.size
    let components := countComponents adj
    let holes := (edges + components) - vertices
    return holes

partial def parseSegments (k : Nat) (nums : List Int) (acc : List Segment)
  : List Segment × List Int :=
  match k with
  | 0 => (List.reverse acc, nums)
  | Nat.succ k' =>
    match nums with
    | x1 :: y1 :: x2 :: y2 :: rest =>
        let seg := Segment.mk ⟨x1, y1⟩ ⟨x2, y2⟩
        parseSegments k' rest (seg :: acc)
    | _ => (List.reverse acc, [])

partial def process : List Int → IO Unit
  | [] => pure ()
  | n :: rest => do
      if n == 0 then
        pure ()
      else
        let nNat := Int.toNat n
        let (segs, rest') := parseSegments nNat rest []
        IO.println (countHoles segs)
        process rest'

def main : IO Unit := do
  let input ← IO.getStdin >>= (·.readToEnd)
  let parts := input.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let nums := parts.filterMap (fun s =>
    if s = "" then none else some (String.toInt! s))
  process nums
