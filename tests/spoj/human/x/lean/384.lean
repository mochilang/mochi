/- Solution for SPOJ FOOL - Any fool can do it
https://www.spoj.com/problems/FOOL/
-/
import Std
open Std

-- Helper to map and concatenate lists
private def bindList {α β} : List α -> (α -> List β) -> List β
| [], _ => []
| x::xs, f => f x ++ bindList xs f

mutual
  partial def parseSet : List Char -> List (List Char)
  | '{'::cs =>
    bindList (parseElementList cs) (fun rest =>
      match rest with
      | '}'::tail => [tail]
      | _ => [])
  | _ => []

  partial def parseElementList : List Char -> List (List Char)
  | cs => cs :: parseList cs

  partial def parseList : List Char -> List (List Char)
  | cs =>
    bindList (parseElement cs) (fun rest =>
      match rest with
      | ','::rest2 => parseList rest2
      | _ => [rest])

  partial def parseElement : List Char -> List (List Char)
  | s@('{'::cs) => parseSet s ++ [cs]
  | '}'::cs => [cs]
  | ','::cs => [cs]
  | _ => []
end

def isSet (s : String) : Bool :=
  (parseSet s.data).any (fun r => r.isEmpty)

partial def solve (h : IO.FS.Stream) (i n : Nat) : IO Unit := do
  if i > n then
    pure ()
  else
    let line ← h.getLine
    let w := line.trim
    let res := if isSet w then "Set" else "No Set"
    IO.println s!"Word #{i}: {res}"
    solve h (i+1) n

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h 1 t
