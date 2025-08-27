/- Solution for SPOJ SOLIT - Solitaire
https://www.spoj.com/problems/SOLIT/
-/

import Std
open Std

-- encode four piece positions (0..63) into a single Nat (sorted)
private def encode (a : Array Nat) : Nat :=
  let s := a.qsort (· < ·)
  (((s.get! 0) * 64 + s.get! 1) * 64 + s.get! 2) * 64 + s.get! 3

-- decode state back to array of four positions (sorted)
private def decode (n : Nat) : Array Nat :=
  let p3 := n % 64
  let n := n / 64
  let p2 := n % 64
  let n := n / 64
  let p1 := n % 64
  let p0 := n / 64
  #[p0, p1, p2, p3]

private def contains (a : Array Nat) (p : Nat) : Bool :=
  a.toList.contains p

private def dirs : List (Int × Int) := [(-1,0),(1,0),(0,-1),(0,1)]

-- generate all states reachable in one move
partial def moves (st : Nat) : List Nat :=
  let pieces := decode st
  let rec loop (i : Nat) (acc : List Nat) : List Nat :=
    if h : i < pieces.size then
      let p := pieces.get! i
      let r := p / 8
      let c := p % 8
      let acc := dirs.foldl (fun acc (d : Int × Int) =>
        let nr := (r : Int) + d.fst
        let nc := (c : Int) + d.snd
        if 0 <= nr ∧ nr < 8 ∧ 0 <= nc ∧ nc < 8 then
          let np := (nr.toNat) * 8 + nc.toNat
          if ¬ contains pieces np then
            let arr := (pieces.set! i np).qsort (· < ·)
            encode arr :: acc
          else
            let jr := nr + d.fst
            let jc := nc + d.snd
            if 0 <= jr ∧ jr < 8 ∧ 0 <= jc ∧ jc < 8 then
              let jp := (jr.toNat) * 8 + jc.toNat
              if ¬ contains pieces jp then
                let arr := (pieces.set! i jp).qsort (· < ·)
                encode arr :: acc
              else acc
            else acc
        else acc) acc dirs
      loop (i+1) acc
    else acc
  loop 0 []

-- BFS search up to depth 8
partial def reachable (start target : Nat) : Bool :=
  if start = target then true else
  let rec bfs (frontier : List Nat) (vis : Std.HashSet Nat) (depth : Nat) : Bool :=
    if frontier.any (· = target) then true
    else if depth = 8 then false
    else
      let (next, vis) := frontier.foldl (fun (acc : List Nat × Std.HashSet Nat) st =>
        let (acc, vis) := acc
        let (acc, vis) := moves st |>.foldl (fun (acc, vis) nxt =>
          if vis.contains nxt then (acc, vis) else (nxt :: acc, vis.insert nxt)) (acc, vis)
        (acc, vis)) ([], vis)
      match next with
      | [] => false
      | _  => bfs next vis (depth + 1)
  bfs [start] (Std.HashSet.empty.insert start) 0

-- read a state from tokens starting at index i
private def readState (toks : Array String) (i : Nat) : (Nat × Nat) :=
  let r1 := toks.get! i |>.toNat!
  let c1 := toks.get! (i+1) |>.toNat!
  let r2 := toks.get! (i+2) |>.toNat!
  let c2 := toks.get! (i+3) |>.toNat!
  let r3 := toks.get! (i+4) |>.toNat!
  let c3 := toks.get! (i+5) |>.toNat!
  let r4 := toks.get! (i+6) |>.toNat!
  let c4 := toks.get! (i+7) |>.toNat!
  let arr : Array Nat := #[(r1-1)*8 + (c1-1), (r2-1)*8 + (c2-1),
                           (r3-1)*8 + (c3-1), (r4-1)*8 + (c4-1)]
  (encode arr, i + 8)

partial def solve (toks : Array String) (i : Nat) (cases : Nat)
  (acc : List String) : List String :=
  if cases = 0 then acc.reverse
  else
    let (s1, i1) := readState toks i
    let (s2, i2) := readState toks i1
    let res := if reachable s1 s2 then "YES" else "NO"
    solve toks i2 (cases - 1) (res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solve toks 1 t []
  for line in outs do
    IO.println line
