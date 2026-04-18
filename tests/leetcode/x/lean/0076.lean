def idx (c : Char) : Nat :=
  if c >= 'A' && c <= 'Z' then c.toNat - 'A'.toNat
  else 26 + (c.toNat - 'a'.toNat)

def makeCounts : Array Int := Array.replicate 52 0

def addChar (arr : Array Int) (c : Char) (delta : Int) : Array Int :=
  let i := idx c
  arr.set! i (arr.getD i 0 + delta)

def covers (cur need : Array Int) : Bool :=
  let rec go (i : Nat) :=
    if i < 52 then
      if cur.getD i 0 < need.getD i 0 then false else go (i + 1)
    else
      true
  go 0

def buildNeed (chars : List Char) : Array Int :=
  chars.foldl (fun acc c => addChar acc c 1) makeCounts

def minWindow (s t : String) : String :=
  let sb := s.toList.toArray
  let need := buildNeed t.toList
  let n := sb.size
  let rec outer (l : Nat) (bestStart bestLen : Nat) : Nat × Nat :=
    if l < n then
      let rec inner (r : Nat) (cur : Array Int) (bestStart bestLen : Nat) : Nat × Nat :=
        if r < n then
          let cur1 := addChar cur (sb.getD r ' ') 1
          if covers cur1 need then
            let len := r - l + 1
            if len < bestLen then (l, len) else (bestStart, bestLen)
          else
            inner (r + 1) cur1 bestStart bestLen
        else
          (bestStart, bestLen)
      let (bs, bl) := inner l makeCounts bestStart bestLen
      outer (l + 1) bs bl
    else
      (bestStart, bestLen)
  let (bestStart, bestLen) := outer 0 0 (n + 1)
  if bestLen > n then "" else String.ofList ((sb.extract bestStart (bestStart + bestLen)).toList)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then
    pure ()
  else do
    let t := (lines.getD 0 "0").toNat!
    let mut out : Array String := #[]
    for i in List.range t do
      out := out.push (minWindow (lines.getD (1 + 2*i) "") (lines.getD (2 + 2*i) ""))
    IO.println (String.intercalate "\n" out.toList)
