import Std

partial def addLists : List Int → List Int → Int → List Int
  | [], [], 0 => []
  | [], [], carry => [carry]
  | x :: xs, [], carry =>
      let s := x + carry
      (s % 10) :: addLists xs [] (s / 10)
  | [], y :: ys, carry =>
      let s := y + carry
      (s % 10) :: addLists [] ys (s / 10)
  | x :: xs, y :: ys, carry =>
      let s := x + y + carry
      (s % 10) :: addLists xs ys (s / 10)

def fmt : List Int → String
  | [] => "[]"
  | x :: xs => "[" ++ toString x ++ String.join (xs.map (fun v => "," ++ toString v)) ++ "]"

def parseTokens (s : String) : List String :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "")

partial def takeInts : Nat → List String → List Int → (List Int × List String)
  | 0, rest, acc => (acc.reverse, rest)
  | n + 1, x :: rest, acc => takeInts n rest (x.toInt! :: acc)
  | _, [], acc => (acc.reverse, [])

partial def solve : Nat → List String → List String
  | 0, _ => []
  | n + 1, x :: rest =>
      let len1 := x.toNat!
      let (a, rest1) := takeInts len1 rest []
      match rest1 with
      | y :: rest2 =>
          let len2 := y.toNat!
          let (b, rest3) := takeInts len2 rest2 []
          fmt (addLists a b 0) :: solve n rest3
      | [] => []
  | _, [] => []

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  match parseTokens data with
  | [] => pure ()
  | tStr :: rest => IO.println (String.intercalate "\n" (solve tStr.toNat! rest))
