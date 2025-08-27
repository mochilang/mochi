/- Solution for SPOJ POLYGAME - The Game of Polygons
https://www.spoj.com/problems/POLYGAME/
-/

import Std
open Std

def sharesEdge (black : Array Nat) (a b c : Nat) : Bool :=
  let inBlack (v : Nat) := v = black[0]! || v = black[1]! || v = black[2]!
  let cnt := (if inBlack a then 1 else 0) + (if inBlack b then 1 else 0) + (if inBlack c then 1 else 0)
  cnt ≥ 2

partial def parseTriangles (k : Nat) (xs : List Nat) (deg : Nat) (black : Array Nat) :
    (Nat × List Nat) :=
  match k with
  | 0 => (deg, xs)
  | Nat.succ k' =>
      match xs with
      | a :: b :: c :: rest =>
          let deg' := if sharesEdge black a b c then deg + 1 else deg
          parseTriangles k' rest deg' black
      | _ => (deg, [])

partial def processCase (xs : List Nat) : (String × List Nat) :=
  match xs with
  | n :: a :: b :: c :: rest =>
      let black : Array Nat := #[a, b, c]
      let (deg, rest') := parseTriangles (n - 3) rest 0 black
      let ans := if deg ≤ 1 || n % 2 = 0 then "YES" else "NO"
      (ans, rest')
  | _ => ("", [])

partial def processCases : Nat → List Nat → List String → List String
| 0, _, acc => acc.reverse
| Nat.succ t, xs, acc =>
  let (ans, xs') := processCase xs
  processCases t xs' (ans :: acc)

def parseInts (s : String) : List Nat :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
   |>.filterMap (fun t => if t.isEmpty then none else t.toNat?)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let ints := parseInts input
  match ints with
  | [] => pure ()
  | t :: rest =>
      let lines := processCases t rest []
      IO.println (String.intercalate "\n" lines)
