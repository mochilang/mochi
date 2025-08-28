/- Solution for SPOJ VHUFFM - Variable Radix Huffman Encoding
https://www.spoj.com/problems/VHUFFM/
-/
import Std
open Std

structure Node where
  freq : Nat
  minIdx : Nat
  idx? : Option Nat
  children : Array Node
deriving Inhabited

def cmpNode (a b : Node) : Bool :=
  if a.freq = b.freq then a.minIdx < b.minIdx else a.freq < b.freq

partial def buildTree (nodes : Array Node) (r : Nat) : Node :=
  if h : nodes.size = 1 then
    nodes[0]!
  else
    let sorted := nodes.qsort cmpNode
    let group := sorted.extract 0 r
    let rest := sorted.extract r sorted.size
    let freq := group.foldl (fun s n => s + n.freq) 0
    let minIdx := group.foldl (fun m n => if n.minIdx < m then n.minIdx else m) (group[0]!.minIdx)
    let newNode := {freq := freq, minIdx := minIdx, idx? := none, children := group}
    buildTree (rest.push newNode) r

partial def assignCodes (node : Node) (pref : String) (codes : Array String) : Array String :=
  if node.children.isEmpty then
    match node.idx? with
    | some i => codes.set! i pref
    | none => codes
  else
    let rec loop (i : Nat) (cs : Array String) : Array String :=
      if h : i < node.children.size then
        let ch := Char.ofNat (48 + i)
        let cs := assignCodes (node.children[i]!) (pref.push ch) cs
        loop (i+1) cs
      else
        cs
    loop 0 codes

def solveCase (r n : Nat) (freqs : Array Nat) : String × Array String :=
  let modv := if r = 1 then 0 else (n - 1) % (r - 1)
  let dummy := if modv = 0 then 0 else (r - 1 - modv)
  let total := freqs.foldl (fun s x => s + x) 0
  let nodes := Id.run do
    let mut arr := Array.mkEmpty (n + dummy)
    for i in [0:n] do
      arr := arr.push {freq := freqs[i]!, minIdx := i, idx? := some i, children := #[]}
    for j in [0:dummy] do
      arr := arr.push {freq := 0, minIdx := n + j, idx? := none, children := #[]}
    return arr
  let tree := buildTree nodes r
  let codes := assignCodes tree "" (Array.mkArray n "")
  let sumLen := Id.run do
    let mut s := 0
    for i in [0:n] do
      s := s + (codes[i]!).length * freqs[i]!
    return s
  let avg100 := (200 * sumLen + total) / (2 * total)
  let intPart := avg100 / 100
  let fracPart := avg100 % 100
  let avgStr := s!"{intPart}.{if fracPart < 10 then "0" else ""}{fracPart}"
  (avgStr, codes)

partial def parse (nums : Array Nat) (idx caseNo : Nat) (acc : Array String) : Array String :=
  if nums[idx]! = 0 then
    acc
  else
    let r := nums[idx]!
    let n := nums[idx + 1]!
    let freqs := Id.run do
      let mut arr := Array.mkEmpty n
      for i in [0:n] do
        arr := arr.push (nums[idx + 2 + i]!)
      return arr
    let (avgStr, codes) := solveCase r n freqs
    let acc := Id.run do
      let mut a := acc.push s!"Set {caseNo}; average length {avgStr}"
      for i in [0:n] do
        let letter := Char.ofNat (65 + i)
        a := a.push s!"    {letter}: {codes[i]!}"
      a := a.push ""
      return a
    parse nums (idx + 2 + n) (caseNo + 1) acc

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
  let nums := toks.map (·.toNat!) |> List.toArray
  let res := parse nums 0 1 #[]
  for line in res do
    IO.println line
