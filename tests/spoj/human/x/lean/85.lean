-- https://www.spoj.com/problems/classical/85
import Std

structure Ori where
  top : Nat
  bottom : Nat
  north : Nat
  south : Nat
  east : Nat
  west : Nat
  deriving DecidableEq

structure State where
  x : Int
  y : Int
  o : Ori
  deriving DecidableEq

def rollNorth (o : Ori) : Ori :=
  { top := o.south, bottom := o.north, north := o.top, south := o.bottom, east := o.east, west := o.west }

def rollSouth (o : Ori) : Ori :=
  { top := o.north, bottom := o.south, north := o.bottom, south := o.top, east := o.east, west := o.west }

def rollEast (o : Ori) : Ori :=
  { top := o.west, bottom := o.east, east := o.top, west := o.bottom, north := o.north, south := o.south }

def rollWest (o : Ori) : Ori :=
  { top := o.east, bottom := o.west, east := o.bottom, west := o.top, north := o.north, south := o.south }

structure Node where
  s : State
  cost : Nat

def insertPQ (q : List Node) (n : Node) : List Node :=
  match q with
  | [] => [n]
  | h::t => if n.cost < h.cost then n :: q else h :: insertPQ t n

partial def dijkstra (costs : Array Nat) (start : State) (tx ty : Int) (xmin xmax : Int) : Nat :=
  let rec loop (visited : List Node) (q : List Node) : Nat :=
    match q with
    | [] => 0
    | h::t =>
        let s := h.s
        if s.x == tx && s.y == ty then h.cost
        else if visited.any (fun n => n.s == s && n.cost ≤ h.cost) then
          loop visited t
        else
          let visited := h :: visited
          let neighbors :=
            [(1, rollNorth s.o, s.x, s.y + 1),
             (1, rollSouth s.o, s.x, s.y - 1),
             (1, rollEast s.o, s.x + 1, s.y),
             (1, rollWest s.o, s.x - 1, s.y)]
          let neighbors := neighbors.foldl (fun acc x =>
              let (_, o, nx, ny) := x
              let inRange := ny >= 1 && ny <= 4 && nx >= xmin && nx <= xmax
              if inRange then
                let c := h.cost + costs.get! (o.top-1)
                insertPQ acc {s := {x:=nx, y:=ny, o:=o}, cost := c}
              else acc) t
          loop visited neighbors
  loop [] [ {s := start, cost := 0} ]

def parseInts (s : String) : List Int :=
  s.split (· == ' ') |>.filterMap (·.toInt?)

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let lines := input.trim.splitOn "\n"
  let t := lines.head!.toNat!
  let mut idx := 1
  let mut outputs : List String := []
  for _ in List.range t do
    let nums := parseInts lines[idx]
    idx := idx + 1
    let vals := nums.map (fun n => Int.toNat n)
    let l1 := vals.get! 0; let l2 := vals.get! 1; let l3 := vals.get! 2
    let l4 := vals.get! 3; let l5 := vals.get! 4; let l6 := vals.get! 5
    let costs : Array Nat := #[l1,l2,l3,l4,l5,l6]
    let pos := parseInts lines[idx]
    idx := idx + 1
    let x1 := pos.get! 0; let y1 := pos.get! 1
    let x2 := pos.get! 2; let y2 := pos.get! 3
    let startOri : Ori := {top:=1,bottom:=6,north:=5,south:=2,east:=3,west:=4}
    let start : State := {x:=x1, y:=y1, o:=startOri}
    let xmin := Int.min x1 x2 - 8
    let xmax := Int.max x1 x2 + 8
    let res := dijkstra costs start x2 y2 xmin xmax
    outputs := outputs.concat (toString res)
  IO.println (String.intercalate "\n" outputs)
