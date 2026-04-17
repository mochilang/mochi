import Std

def matchBracket : Char → Char → Bool
  | ')', '(' => true
  | ']', '[' => true
  | '}', '{' => true
  | _, _ => false

def isValidChars : List Char → List Char → Bool
  | [], stack => stack.isEmpty
  | c :: rest, stack =>
      if c == '(' || c == '[' || c == '{' then
        isValidChars rest (c :: stack)
      else
        match stack with
        | op :: tail => if matchBracket c op then isValidChars rest tail else false
        | [] => false

def isValid (s : String) : Bool := isValidChars s.toList []

def parseTokens (s : String) : List String :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "")

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  match parseTokens data with
  | [] => pure ()
  | tStr :: rest =>
      let t := tStr.toNat!
      let lines := (rest.take t).map (fun s => if isValid s then "true" else "false")
      IO.println (String.intercalate "\n" lines)
