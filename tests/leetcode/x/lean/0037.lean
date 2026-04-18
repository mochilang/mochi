def getCell (b : Array (Array Char)) (r c : Nat) : Char :=
  (b.getD r #[]).getD c ' '

def valid (b : Array (Array Char)) (r c : Nat) (ch : Char) : Bool :=
  let br := (r / 3) * 3
  let bc := (c / 3) * 3
  (List.range 9).all (fun i => getCell b r i != ch && getCell b i c != ch) &&
  (List.range 3).all (fun di => (List.range 3).all (fun dj => getCell b (br + di) (bc + dj) != ch))

partial def solve (b : Array (Array Char)) : Option (Array (Array Char)) :=
  let empties :=
    (List.range 9).flatMap (fun r =>
      (List.range 9).filterMap (fun c => if getCell b r c = '.' then some (r, c) else none))
  match empties with
  | [] => some b
  | (r, c) :: _ =>
      let rec tryChars (chars : List Char) : Option (Array (Array Char)) :=
        match chars with
        | [] => none
        | ch :: rest =>
            if valid b r c ch then
              let row := (b.getD r #[]).set! c ch
              match solve (b.set! r row) with
              | some ans => some ans
              | none => tryChars rest
            else
              tryChars rest
      tryChars ['1', '2', '3', '4', '5', '6', '7', '8', '9']

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).filter (fun line => line != "") |>.toArray
  if lines.isEmpty then
    pure ()
  else
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let b := (lines.extract idx (idx + 9)).map (fun s => s.toList.toArray)
      idx := idx + 9
      match solve b with
      | some solved =>
          for row in solved do
            out := out.push (String.ofList row.toList)
      | none => pure ()
    IO.println (String.intercalate "\n" out.toList)
