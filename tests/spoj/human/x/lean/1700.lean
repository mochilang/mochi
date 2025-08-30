/- Solution for SPOJ TRSTAGE - Traveling by Stagecoach
https://www.spoj.com/problems/TRSTAGE/
-/
import Std
open Std

/-- format float with three decimals --/
private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

/-- insert into a priority queue represented as a sorted list --/
private def insertPQ (p : Float × Nat × Nat) (pq : List (Float × Nat × Nat)) : List (Float × Nat × Nat) :=
  match pq with
  | [] => [p]
  | q :: qs =>
      let (d1,_,_) := p
      let (d2,_,_) := q
      if d1 ≤ d2 then p :: pq else q :: insertPQ p qs

/-- run Dijkstra on state graph (city, usedTicketsMask) --/
partial def dijkstra (m n : Nat) (tickets : Array Nat) (adj : Array (List (Nat × Nat))) (start : Nat) : Array Float :=
  Id.run do
    let m1 := m + 1
    let states := m1 * (1 <<< n)
    let inf : Float := 1e18
    let idx := fun (city mask : Nat) => mask * m1 + city
    let mut dist := Array.mkArray states inf
    dist := dist.set! (idx start 0) 0.0
    let mut pq : List (Float × Nat × Nat) := []
    pq := insertPQ (0.0, start, 0) pq
    let rec loop (dist : Array Float) (pq : List (Float × Nat × Nat)) : Array Float :=
      match pq with
      | [] => dist
      | (d, city, mask) :: qs =>
          let sIdx := idx city mask
          if d > dist.get! sIdx then
            loop dist qs
          else
            let mut dist := dist
            let mut pq := qs
            for (to,len) in adj.get! city do
              for i in [0:n] do
                if (mask &&& (1 <<< i)) = 0 then
                  let newMask := mask ||| (1 <<< i)
                  let nd := d + (Float.ofNat len) / (Float.ofNat (tickets.get! i))
                  let idx2 := idx to newMask
                  if nd < dist.get! idx2 then
                    dist := dist.set! idx2 nd
                    pq := insertPQ (nd, to, newMask) pq
            loop dist pq
    loop dist pq

/-- build adjacency list from edge tokens --/
partial def buildEdges (es : List Nat) (adj : Array (List (Nat × Nat))) : Array (List (Nat × Nat)) :=
  match es with
  | x :: y :: z :: rest =>
      let adj := adj.set! x ((y,z) :: adj.get! x)
      let adj := adj.set! y ((x,z) :: adj.get! y)
      buildEdges rest adj
  | _ => adj

/-- solve recursively over all datasets --/
partial def solveTokens (toks : List Nat) (acc : List String) : List String :=
  match toks with
  | n :: m :: p :: a :: b :: rest =>
      if n = 0 && m = 0 && p = 0 && a = 0 && b = 0 then
        acc.reverse
      else
        let tickets := rest.take n
        let rest1 := rest.drop n
        let edgeNums := rest1.take (p*3)
        let rest2 := rest1.drop (p*3)
        let adj0 : Array (List (Nat × Nat)) := Array.mkArray (m+1) []
        let adj := buildEdges edgeNums adj0
        let dist := dijkstra m n tickets.toArray adj a
        let m1 := m + 1
        let idx := fun (city mask : Nat) => mask * m1 + city
        let inf : Float := 1e18
        let mut best := inf
        for mask in [0:(1 <<< n)] do
          let d := dist.get! (idx b mask)
          if d < best then best := d
        let ans :=
          if best >= inf/2 then "Impossible" else format3 best
        solveTokens rest2 (ans :: acc)
  | _ => acc.reverse

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let nums := input.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |>.map String.toNat!
  let outputs := solveTokens nums []
  for line in outputs do
    IO.println line
