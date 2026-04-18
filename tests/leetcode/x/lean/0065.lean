def isDigit (c : Char) : Bool := c >= '0' && c <= '9'

def isNumber (s : String) : Bool :=
  let chars := s.toList.toArray
  let rec go (i : Nat) (seenDigit seenDot seenExp digitAfterExp : Bool) : Bool :=
    if i < chars.size then
      let ch := chars.getD i ' '
      if isDigit ch then
        go (i + 1) true seenDot seenExp (if seenExp then true else digitAfterExp)
      else if ch = '+' || ch = '-' then
        if i != 0 && chars.getD (i - 1) ' ' != 'e' && chars.getD (i - 1) ' ' != 'E' then
          false
        else
          go (i + 1) seenDigit seenDot seenExp digitAfterExp
      else if ch = '.' then
        if seenDot || seenExp then false else go (i + 1) seenDigit true seenExp digitAfterExp
      else if ch = 'e' || ch = 'E' then
        if seenExp || !seenDigit then false else go (i + 1) seenDigit seenDot true false
      else
        false
    else
      seenDigit && digitAfterExp
  go 0 false false false true

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then
    pure ()
  else do
    let t := ((lines.getD 0 "0").trimAscii).toNat!
    let mut out : Array String := #[]
    for i in List.range t do
      out := out.push (if isNumber (lines.getD (i + 1) "") then "true" else "false")
    IO.println (String.intercalate "\n" out.toList)
