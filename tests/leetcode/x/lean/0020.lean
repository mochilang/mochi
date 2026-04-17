import Std

def matchBracket : Char → Char → Bool
  | ')', '(' => true
  | ']', '[' => true
  | '}', '{' => true
  | _, _ => false

def isValidChars : List Char → List Char → Bool
  | [], stack => stack.isEmpty
  | c :: rest, stack =>
      if c = '(' || c = '[' || c = '{' then
        isValidChars rest (c :: stack)
      else
        match stack with
        | open :: tail => if matchBracket c open then isValidChars rest tail else false
        | [] => false

def isValid (s : String) : Bool := isValidChars s.data []

def parseTokens (s : String) : List String :=
  (s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun tok => tok ≠ "")

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  match parseTokens data with
  | [] => pure ()
  | t :: rest =>
      let lines := (rest.take t.toNat!).map (fun s => if isValid s then "true" else "false")
      IO.println (String.intercalate "\n" lines)
