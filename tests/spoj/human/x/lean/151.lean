/- Solution for SPOJ COURIER - The Courier
https://www.spoj.com/problems/COURIER/
-/

import Std
open Std

def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

partial def parseEdges : Nat -> List Nat -> List (Nat × Nat × Nat) -> (List (Nat × Nat × Nat) × List Nat)
| 0, xs, acc => (acc.reverse, xs)
| Nat.succ m, u :: v :: d :: rest, acc => parseEdges m rest ((u, v, d) :: acc)
| _, xs, acc => (acc.reverse, xs)

partial def parseOrders : Nat -> List Nat -> List (Nat × Nat × Nat) -> (List (Nat × Nat × Nat) × List Nat)
| 0, xs, acc => (acc.reverse, xs)
| Nat.succ z, u :: v :: b :: rest, acc => parseOrders z rest ((u, v, b) :: acc)
| _, xs, acc => (acc.reverse, xs)

def solve (n b : Nat) (edges orders : List (Nat × Nat × Nat)) : Nat :=
  Id.run do
    let inf := 1000000000
    -- distance matrix
    let mut dist := Array.replicate (n+1) (Array.replicate (n+1) inf)
    for i in [1:n+1] do
      dist := dist.set! i ((dist[i]!).set! i 0)
    for (u, v, d) in edges do
      let rowU := dist[u]!
      let rowU := rowU.set! v (Nat.min (rowU[v]!) d)
      dist := dist.set! u rowU
      let rowV := dist[v]!
      let rowV := rowV.set! u (Nat.min (rowV[u]!) d)
      dist := dist.set! v rowV
    -- Floyd-Warshall
    for k in [1:n+1] do
      for i in [1:n+1] do
        let dik := dist[i]![k]!
        if dik < inf then
          for j in [1:n+1] do
            let new := dik + dist[k]![j]!
            if new < dist[i]![j]! then
              let row := dist[i]!
              dist := dist.set! i (row.set! j new)
    -- expand orders into individual parcels
    let mut froms : Array Nat := #[]
    let mut tos : Array Nat := #[]
    for (u, v, cnt) in orders do
      for _ in [0:cnt] do
        froms := froms.push u
        tos := tos.push v
    let t := froms.size
    let totalStates := (1 <<< t)
    let mut dp := Array.replicate totalStates (Array.replicate (n+1) inf)
    dp := dp.set! 0 ((dp[0]!).set! b 0)
    for mask in [0:totalStates] do
      let row := dp[mask]!
      for pos in [1:n+1] do
        let cur := row[pos]!
        if cur < inf then
          for k in [0:t] do
            if (mask &&& (1 <<< k)) = 0 then
              let u := froms[k]!
              let v := tos[k]!
              let cost := cur + dist[pos]![u]! + dist[u]![v]!
              let nm := mask ||| (1 <<< k)
              let arr := dp[nm]!
              let prev := arr[v]!
              if cost < prev then
                dp := dp.set! nm (arr.set! v cost)
    let fullMask := totalStates - 1
    let finalRow := dp[fullMask]!
    let mut ans := inf
    for pos in [1:n+1] do
      let total := finalRow[pos]! + dist[pos]![b]!
      if total < ans then ans := total
    return ans

partial def processCases : Nat -> List Nat -> List String -> List String
| 0, _, acc => acc.reverse
| Nat.succ t, xs, acc =>
  match xs with
  | n :: m :: b :: rest =>
      let (edges, rest1) := parseEdges m rest []
      match rest1 with
      | z :: rest2 =>
          let (orders, rest3) := parseOrders z rest2 []
          let ans := solve n b edges orders
          processCases t rest3 (toString ans :: acc)
      | _ => acc.reverse
  | _ => acc.reverse

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let ints := parseInts input
  match ints with
  | [] => pure ()
  | t :: rest =>
      let lines := processCases t rest []
      IO.println (String.intercalate "\n" lines)
