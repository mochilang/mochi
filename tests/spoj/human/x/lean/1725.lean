/- Solution for SPOJ IMPORT1 - The Importance
https://www.spoj.com/problems/IMPORT1/
-/
import Std
open Std

partial def readInts : IO (Array Int) := do
  let h ← IO.getStdin
  let rec readAll (buf : ByteArray) : IO ByteArray := do
    let chunk ← h.read 1024
    if chunk.isEmpty then
      pure buf
    else
      readAll (buf ++ chunk)
  let bytes ← readAll ByteArray.empty
  let data := String.fromUTF8! bytes
  let parts := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let arr := (s.splitOn ".").toArray
  let intPart := arr[0]!
  let fracPart :=
    if arr.size > 1 then
      let f := arr[1]!
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

private def importance (n : Nat) (adj : Array (Array (Nat × Int))) : Array Float :=
  Id.run do
    let inf : Int := 10^15
    let mut res : Array Float := Array.replicate (n+1) 0.0
    for s in [1:n+1] do
      let mut dist : Array Int := Array.replicate (n+1) inf
      let mut sigma : Array Float := Array.replicate (n+1) 0.0
      let mut pred : Array (List Nat) := Array.replicate (n+1) []
      let mut visited : Array Bool := Array.replicate (n+1) false
      dist := dist.set! s 0
      sigma := sigma.set! s 1.0
      let mut stack : Array Nat := #[]
      for _ in [0:n] do
        let mut v : Nat := 0
        let mut best : Int := inf
        for i in [1:n+1] do
          if !visited[i]! && dist[i]! < best then
            best := dist[i]!
            v := i
        if best != inf then
          visited := visited.set! v true
          stack := stack.push v
          for (to,w) in adj[v]! do
            let alt := dist[v]! + w
            if alt < dist[to]! then
              dist := dist.set! to alt
              sigma := sigma.set! to sigma[v]!
              pred := pred.set! to [v]
            else if alt == dist[to]! then
              sigma := sigma.set! to (sigma[to]! + sigma[v]!)
              pred := pred.modify to (fun l => v :: l)
      let mut delta : Array Float := Array.replicate (n+1) 0.0
      for w in stack.reverse do
        for v in pred[w]! do
          let add := (sigma[v]! / sigma[w]!) * (1.0 + delta[w]!)
          delta := delta.set! v (delta[v]! + add)
        if w ≠ s then
          res := res.set! w (res[w]! + delta[w]!)
    return res

def main : IO Unit := do
  let toks ← readInts
  let mut idx : Nat := 0
  let t := toks[idx]!.toNat; idx := idx + 1
  for _ in [0:t] do
    let n := toks[idx]!.toNat
    let m := toks[idx+1]!.toNat
    idx := idx + 2
    let mut adj : Array (Array (Nat × Int)) := Array.replicate (n+1) #[]
    for _ in [0:m] do
      let a := toks[idx]!.toNat
      let b := toks[idx+1]!.toNat
      let c := toks[idx+2]!
      idx := idx + 3
      adj := adj.modify a (fun arr => arr.push (b,c))
      adj := adj.modify b (fun arr => arr.push (a,c))
    let res := importance n adj
    for i in [1:n+1] do
      IO.println (format3 res[i]!)
  pure ()
