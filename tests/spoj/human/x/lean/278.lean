/- Solution for SPOJ BICYCLE - Bicycle
https://www.spoj.com/problems/BICYCLE/
-/

import Std
open Std

structure Circle where
  x : Float
  y : Float
  r : Float

def pointOn (x y : Float) (c : Circle) : Bool :=
  let dx := x - c.x
  let dy := y - c.y
  Float.abs (Float.sqrt (dx*dx + dy*dy) - c.r) < 1e-6

structure Node where
  x : Float
  y : Float
  on1 : Bool
  on2 : Bool
  deriving Inhabited

def addNode (nodes : Array Node) (x y : Float) (on1 on2 : Bool) : (Array Node × Nat) :=
  let rec loop (i : Nat) (arr : Array Node) : (Array Node × Nat) :=
    if h : i < arr.size then
      let p := arr.get! i
      if Float.abs (p.x - x) < 1e-7 && Float.abs (p.y - y) < 1e-7 then
        (arr.set! i {p with on1 := p.on1 || on1, on2 := p.on2 || on2}, i)
      else
        loop (i+1) arr
    else
      let idx := arr.size
      (arr.push {x := x, y := y, on1 := on1, on2 := on2}, idx)
  loop 0 nodes

def piF : Float := 3.141592653589793

def angle (c : Circle) (p : Node) : Float :=
  let t := Float.atan2 (p.y - c.y) (p.x - c.x)
  let t := if t < 0 then t + 2.0 * piF else t
  t

def arc (c : Circle) (a b : Node) : Float :=
  let t1 := angle c a
  let t2 := angle c b
  let diff := Float.abs (t1 - t2)
  let diff := if diff > piF then 2.0 * piF - diff else diff
  diff * c.r

def intersections (c1 c2 : Circle) : Array (Float × Float) :=
  let dx := c2.x - c1.x
  let dy := c2.y - c1.y
  let d := Float.sqrt (dx*dx + dy*dy)
  if d < 1e-8 then
    #[]
  else if d > c1.r + c2.r + 1e-8 || d < Float.abs (c1.r - c2.r) - 1e-8 then
    #[]
  else
    let a := (c1.r*c1.r - c2.r*c2.r + d*d) / (2.0*d)
    let h2 := c1.r*c1.r - a*a
    if h2 < -1e-8 then
      #[]
    else
      let h := Float.sqrt (if h2 < 0 then 0 else h2)
      let xm := c1.x + a*dx/d
      let ym := c1.y + a*dy/d
      if h < 1e-8 then
        #[(xm, ym)]
      else
        let rx := -dy * h / d
        let ry := dx * h / d
        #[(xm + rx, ym + ry), (xm - rx, ym - ry)]

def updateEdge (dist : Array (Array Float)) (i j : Nat) (d : Float) : Array (Array Float) :=
  let dist := dist.modify i (fun row => row.modify j (fun old => if d < old then d else old))
  dist.modify j (fun row => row.modify i (fun old => if d < old then d else old))

def parseFloat (s : String) : Float :=
  let s := s.trim
  let sign := if s.startsWith "-" then -1.0 else 1.0
  let s := if s.startsWith "-" then s.drop 1 else s
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let intVal := intPart.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let fracPart := if h : parts.length > 1 then parts.get! 1 else ""
  let fracVal := fracPart.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let fracLen := fracPart.length
  let denom := Nat.pow 10 fracLen
  sign * (Float.ofNat intVal + Float.ofNat fracVal / Float.ofNat denom)

def format10 (x : Float) : String :=
  let scaled := (Float.floor (x * 10000000000.0 + 0.5)).toUInt64.toNat
  let intPart := scaled / 10000000000
  let fracPart := scaled % 10000000000
  let fracStr := toString fracPart
  let pad := String.mk (List.replicate (10 - fracStr.length) '0')
  s!"{intPart}.{pad}{fracStr}"

private def solveCase (c1 c2 : Circle) (p1 p2 : (Float × Float)) : String :=
  Id.run do
    let (nodes0, sIdx) := addNode #[] p1.1 p1.2 (pointOn p1.1 p1.2 c1) (pointOn p1.1 p1.2 c2)
    let (nodes1, eIdx) := addNode nodes0 p2.1 p2.2 (pointOn p2.1 p2.2 c1) (pointOn p2.1 p2.2 c2)
    let mut nodes := nodes1
    for (x,y) in intersections c1 c2 do
      let (ns, _) := addNode nodes x y true true
      nodes := ns
    let n := nodes.size
    let inf : Float := 1e18
    let mut dist : Array (Array Float) := Array.mkArray n (Array.mkArray n inf)
    for i in [0:n] do
      dist := dist.modify i (fun row => row.set! i 0)
    for i in [0:n] do
      for j in [i+1:n] do
        let ni := nodes.get! i
        let nj := nodes.get! j
        let mut best : Option Float := none
        if ni.on1 && nj.on1 then
          let d := arc c1 ni nj
          best := some (match best with | none => d | some b => if d < b then d else b)
        if ni.on2 && nj.on2 then
          let d := arc c2 ni nj
          best := some (match best with | none => d | some b => if d < b then d else b)
        match best with
        | some d => dist := updateEdge dist i j d
        | none => pure ()
    for k in [0:n] do
      for i in [0:n] do
        for j in [0:n] do
          let dik := dist.get! i |>.get! k
          let dkj := dist.get! k |>.get! j
          let dij := dist.get! i |>.get! j
          let nd := dik + dkj
          if nd < dij then
            dist := dist.modify i (fun row => row.set! j nd)
            dist := dist.modify j (fun row => row.set! i nd)
    let ans := dist.get! sIdx |>.get! eIdx
    if ans >= inf / 2.0 then
      "-1"
    else
      return format10 ans

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line1 ← h.getLine
    let l1 := line1.trim
    if l1 = "" then
      loop h n
    else
      let nums1 := l1.split (· = ' ') |>.filter (· ≠ "") |>.map parseFloat
      let line2 ← h.getLine
      let nums2 := line2.trim.split (· = ' ') |>.filter (· ≠ "") |>.map parseFloat
      let line3 ← h.getLine
      let nums3 := line3.trim.split (· = ' ') |>.filter (· ≠ "") |>.map parseFloat
      let line4 ← h.getLine
      let nums4 := line4.trim.split (· = ' ') |>.filter (· ≠ "") |>.map parseFloat
      let c1 := {x := nums1.get! 0, y := nums1.get! 1, r := nums1.get! 2}
      let c2 := {x := nums2.get! 0, y := nums2.get! 1, r := nums2.get! 2}
      let p1 := (nums3.get! 0, nums3.get! 1)
      let p2 := (nums4.get! 0, nums4.get! 1)
      IO.println (solveCase c1 c2 p1 p2)
      loop h (n-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
