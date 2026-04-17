import Std

def value : Char → Int
  | 'I' => 1 | 'V' => 5 | 'X' => 10 | 'L' => 50 | 'C' => 100 | 'D' => 500 | 'M' => 1000 | _ => 0

def romanToIntChars : List Char → Int
  | [] => 0
  | [c] => value c
  | c :: n :: rest =>
      (if value c < value n then - value c else value c) + romanToIntChars (n :: rest)

def romanToInt (s : String) : Int := romanToIntChars s.toList

def parseTokens (s : String) : List String :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "")

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let tokens := parseTokens data
  match tokens with
  | [] => pure ()
  | tStr :: rest =>
      let t := tStr.toNat!
      let lines := (rest.take t).map (fun s => toString (romanToInt s))
      IO.println (String.intercalate "\n" lines)
