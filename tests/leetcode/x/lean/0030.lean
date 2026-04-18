partial def sortStrings : List String -> List String
| [] => []
| x :: xs =>
    let smaller := sortStrings (xs.filter (fun y => y <= x))
    let larger := sortStrings (xs.filter (fun y => y > x))
    smaller ++ [x] ++ larger

def fmtList (xs : List Nat) : String := "[" ++ String.intercalate "," (xs.map toString) ++ "]"

def sliceWord (s : String) (start len : Nat) : String := String.ofList ((s.toList.drop start).take len)

def solveCase (s : String) (words : List String) : List Nat :=
  match words with
  | [] => []
  | w :: _ =>
      let wlen := w.length
      let total := wlen * words.length
      let target := sortStrings words
      if s.length < total then
        []
      else
        (List.range (s.length - total + 1)).filter (fun i =>
          let parts := sortStrings ((List.range words.length).map (fun j => sliceWord s (i + j * wlen) wlen))
          parts = target)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := (((String.splitOn input "\n").map fun s => (s.trimAscii).toString).filter (fun line => line != "")).toArray
  if lines.isEmpty then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let s := lines.getD idx ""; idx := idx + 1
      let m := (lines.getD idx "0").toNat!; idx := idx + 1
      let mut words : List String := []
      for _ in [0:m] do words := words ++ [lines.getD idx ""]; idx := idx + 1
      out := out.push (fmtList (solveCase s words))
    IO.println (String.intercalate "\n" out.toList)
