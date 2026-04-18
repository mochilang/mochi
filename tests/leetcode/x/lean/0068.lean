def joinWithSingleSpaces : List String → String
  | [] => ""
  | [w] => w
  | w :: ws => w ++ " " ++ joinWithSingleSpaces ws

def spaces : Nat → String := fun n => String.ofList (List.replicate n ' ')

def buildFull : List String → Nat → Nat → String
  | [w], _, _ => w
  | w :: ws, base, extra => w ++ spaces (base + if extra > 0 then 1 else 0) ++ buildFull ws base (if extra > 0 then extra - 1 else 0)
  | [], _, _ => ""

partial def takeLine (words : Array String) (start maxWidth : Nat) : Nat × Nat :=
  let rec go (j total : Nat) :=
    if j < words.size && total + (words.getD j "").length + (j - start) <= maxWidth then go (j + 1) (total + (words.getD j "").length) else (j, total)
  go start 0

partial def justify (words : Array String) (maxWidth : Nat) : List String :=
  let rec go (i : Nat) : List String :=
    if i >= words.size then [] else
      let (j, total) := takeLine words i maxWidth
      let lineWords := (List.range (j - i)).map (fun k => words.getD (i + k) "")
      let gaps := j - i - 1
      let line :=
        if j = words.size || gaps = 0 then let s := joinWithSingleSpaces lineWords; s ++ spaces (maxWidth - s.length)
        else let spaceTotal := maxWidth - total; buildFull lineWords (spaceTotal / gaps) (spaceTotal % gaps)
      line :: go j
  go 0

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else do
    let mut idx := 0
    let t := (lines.getD idx "0").toNat!; idx := idx + 1
    let mut out : Array String := #[]
    for tc in List.range t do
      let n := (lines.getD idx "0").toNat!; idx := idx + 1
      let words := ((List.range n).map (fun k => lines.getD (idx + k) "")).toArray; idx := idx + n
      let width := (lines.getD idx "0").toNat!; idx := idx + 1
      let ans := justify words width
      out := out.push (toString ans.length)
      for s in ans do out := out.push s!"|{s}|"
      if tc + 1 < t then out := out.push "="
    IO.println (String.intercalate "\n" out.toList)
