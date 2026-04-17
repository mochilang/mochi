import Std

def parseTokens (s : String) : List String :=
  (s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun tok => tok ≠ "")

def startsWith (s p : String) : Bool := p.length ≤ s.length && s.take p.length = p

partial def lcpLoop (p : String) (strs : List String) : String :=
  if strs.all (fun s => startsWith s p) then p else lcpLoop (p.dropRight 1) strs

def lcp (strs : List String) : String :=
  match strs with
  | [] => ""
  | x :: xs => lcpLoop x (x :: xs)

partial def solve : List String → Nat → List String → List String
  | _, 0, acc => acc.reverse
  | n :: rest, t + 1, acc =>
      let k := n.toNat!
      let strs := rest.take k
      let tail := rest.drop k
      solve tail t (("\"" ++ lcp strs ++ "\"") :: acc)
  | [], _, acc => acc.reverse

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  match parseTokens data with
  | [] => pure ()
  | t :: rest => IO.println (String.intercalate "\n" (solve rest t.toNat! []))
