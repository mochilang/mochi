/- Solution for SPOJ SCRAPER - Skyscraper Floors
https://www.spoj.com/problems/SCRAPER/
-/
import Std
open Std

structure Elevator where
  x : Nat
  y : Nat

def stopsOn (e : Elevator) (f : Nat) : Bool :=
  f >= e.y && (f - e.y) % e.x == 0

partial def egcd (a b : Int) : (Int × Int × Int) :=
  if a == 0 then (b, 0, 1)
  else
    let (g, x1, y1) := egcd (b % a) a
    (g, y1 - (b / a) * x1, x1)

def modInv (a m : Int) : Int :=
  let (g, x, _) := egcd a m
  if g != 1 then 0 else
    let x := x % m
    if x < 0 then x + m else x

def intersect (e1 e2 : Elevator) (F : Nat) : Bool :=
  let x1 := Int.ofNat e1.x
  let y1 := Int.ofNat e1.y
  let x2 := Int.ofNat e2.x
  let y2 := Int.ofNat e2.y
  let g := Int.gcd x1 x2
  if (y2 - y1) % g != 0 then false else
    let a1 := x1 / g
    let a2 := x2 / g
    let diff := (y2 - y1) / g
    let inv := modInv a1 a2
    let k := (diff * inv) % a2
    let f0 := y1 + k * x1
    let lcm := x1 / g * x2
    let m := max y1 y2
    let f :=
      if f0 < m then
        let add := (m - f0 + lcm - 1) / lcm
        f0 + add * lcm
      else f0
    f < Int.ofNat F

def buildAdj (es : Array Elevator) (F : Nat) : Array (Array Nat) :=
  Id.run do
    let n := es.size
    let mut adj : Array (Array Nat) := Array.mkArray n #[]
    for i in [0:n] do
      for j in [i+1:n] do
        if intersect (es.get! i) (es.get! j) F then
          adj := adj.modify i (fun arr => arr.push j)
          adj := adj.modify j (fun arr => arr.push i)
    pure adj

partial def bfs (adj : Array (Array Nat)) (start target : List Nat) : Bool :=
  let targetSet := Std.HashSet.ofList target
  let rec loop (q : List Nat) (vis : Std.HashSet Nat) : Bool :=
    match q with
    | [] => false
    | i :: qs =>
      if vis.contains i then
        loop qs vis
      else if targetSet.contains i then
        true
      else
        let vis := vis.insert i
        let neigh := (adj.get! i).toList
        loop (qs ++ neigh) vis
  loop start Std.HashSet.empty

def solveCase (F E A B : Nat) (es : Array Elevator) : Bool :=
  if A == B then true else
    let starts := List.filter (fun i => stopsOn (es.get! i) A) (List.range E)
    let targets := List.filter (fun i => stopsOn (es.get! i) B) (List.range E)
    if starts.isEmpty || targets.isEmpty then false else
      let adj := buildAdj es F
      bfs adj starts targets

partial def readElevators (toks : Array String) (idx : Nat) (e : Nat)
    (arr : Array Elevator) : (Array Elevator × Nat) :=
  if e == 0 then (arr, idx) else
    let x := toks.get! idx |>.toNat!
    let y := toks.get! (idx+1) |>.toNat!
    readElevators toks (idx+2) (e-1) (arr.push {x := x, y := y})

partial def solveAll (toks : Array String) (idx n : Nat) (acc : List String) : List String :=
  if n == 0 then acc.reverse else
    let F := toks.get! idx |>.toNat!
    let E := toks.get! (idx+1) |>.toNat!
    let A := toks.get! (idx+2) |>.toNat!
    let B := toks.get! (idx+3) |>.toNat!
    let (es, nxt) := readElevators toks (idx+4) E #[]
    let ok := solveCase F E A B es
    let line := if ok then
      "It is possible to move the furniture."
    else
      "The furniture cannot be moved."
    solveAll toks nxt (n-1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let n := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 n []
  for line in outs do
    IO.println line
