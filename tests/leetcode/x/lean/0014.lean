import Std

def parseTokens (s : String) : List String :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "")

def startsWith (s p : String) : Bool := p.length ≤ s.length && (s.take p.length |>.toString) == p

partial def lcpLoop (p : String) (strs : List String) : String :=
  if strs.all (fun s => startsWith s p) then p 
  else if p.length == 0 then ""
  else lcpLoop (p.take (p.length - 1) |>.toString) strs

def lcp (strs : List String) : String :=
  match strs with
  | [] => ""
  | x :: xs => lcpLoop x (x :: xs)

partial def solve : List String → Nat → List String → List String
  | _, 0, acc => acc.reverse
  | tokens, t + 1, acc =>
      match tokens with
      | nStr :: rest =>
          let k := nStr.toNat!
          let strs := rest.take k
          let tail := rest.drop k
          solve tail t (("\"" ++ lcp strs ++ "\"") :: acc)
      | [] => acc.reverse

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  match parseTokens data with
  | [] => pure ()
  | tStr :: rest => IO.println (String.intercalate "\n" (solve rest tStr.toNat! []))
