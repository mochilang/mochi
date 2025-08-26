/- Solution for SPOJ JASIEK - Jasiek
https://www.spoj.com/problems/JASIEK/
-/

import Std
open Std

partial def parseCases (ls : List String) (acc : List (List Char)) : List (List Char) :=
  match ls with
  | [] => acc.reverse
  | "P" :: rest =>
      let rec collect (ls : List String) (moves : List Char) :=
        match ls with
        | [] => (moves.reverse, [])
        | "K" :: rest => (moves.reverse, rest)
        | l :: rest =>
            let c := l.trim.data.head!
            collect rest (c :: moves)
      let (moves, rest') := collect rest []
      parseCases rest' (moves :: acc)
  | _ :: rest => parseCases rest acc

def solve (moves : List Char) : Nat :=
  let rec loop (x y : Int) (area2 : Int) (b : Nat) (ms : List Char) : Nat :=
    match ms with
    | [] =>
        let a := Int.natAbs area2
        a / 2 + b / 2 + 1
    | m :: ms' =>
        let (nx, ny) :=
          match m with
          | 'N' => (x, y + 1)
          | 'S' => (x, y - 1)
          | 'E' => (x + 1, y)
          | 'W' => (x - 1, y)
          | _ => (x, y)
        let area2' := area2 + x * ny - y * nx
        loop nx ny area2' (b + 1) ms'
  loop 0 0 0 0 moves

def main : IO Unit := do
  let input ← IO.readStdin
  let lines := input.trim.split (fun c => c = '\n' || c = '\r') |>.filter (fun s => s ≠ "")
  let cases := parseCases lines []
  for moves in cases do
    IO.println (solve moves)
