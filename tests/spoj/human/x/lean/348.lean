/- Solution for SPOJ EXPEDI - Expedition
https://www.spoj.com/problems/EXPEDI/
-/


structure MaxHeap where
  data : Array Nat

namespace MaxHeap

instance : Inhabited MaxHeap := ⟨⟨#[]⟩⟩

def empty : MaxHeap := ⟨#[]⟩

private def swap! (a : Array Nat) (i j : Nat) : Array Nat := Id.run do
  let tmp := a.get! i
  let a := a.set! i (a.get! j)
  let a := a.set! j tmp
  return a

partial def push (h : MaxHeap) (x : Nat) : MaxHeap := Id.run do
  let mut arr := h.data.push x
  let mut i := arr.size - 1
  while i > 0 do
    let parent := (i - 1) / 2
    let vi := arr.get! i
    let vp := arr.get! parent
    if vi > vp then
      arr := arr.set! i vp
      arr := arr.set! parent vi
      i := parent
    else
      break
  return ⟨arr⟩

partial def pop? (h : MaxHeap) : Option (Nat × MaxHeap) :=
  if h.data.isEmpty then none else
    let top := h.data.get! 0
    let size := h.data.size
    if size = 1 then
      some (top, ⟨#[]⟩)
    else
      let arrFinal := Id.run do
        let mut arr := h.data.set! 0 (h.data.get! (size-1))
        arr := arr.pop
        let mut i := 0
        let size := arr.size
        while true do
          let left := 2*i+1
          if left ≥ size then break
          let right := left+1
          let c := if right < size && arr.get! right > arr.get! left then right else left
          if arr.get! c > arr.get! i then
            arr := swap! arr i c
            i := c
          else
            break
        return arr
      some (top, ⟨arrFinal⟩)

end MaxHeap

partial def solveCase (stops : Array (Nat × Nat)) (L P : Nat) : String :=
  Id.run do
    let mut arr : Array (Nat × Nat) := #[]
    for (d,f) in stops do
      arr := arr.push (L - d, f)
    arr := arr.push (L, 0)
    let sorted := (arr.toList.mergeSort (fun a b => a.fst < b.fst)).toArray
    let mut heap : MaxHeap := MaxHeap.empty
    let mut fuel := P
    let mut prev := 0
    let mut ans := 0
    let mut ok := true
    for (dist,f) in sorted do
      let need := dist - prev
      while fuel < need do
        match MaxHeap.pop? heap with
        | none =>
            ok := false
            break
        | some (add, h1) =>
            fuel := fuel + add
            ans := ans + 1
            heap := h1
      if !ok then break
      fuel := fuel - need
      prev := dist
      heap := MaxHeap.push heap f
    return if ok then toString ans else "-1"

partial def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks[0]!.toNat!
    let mut idx := 1
    let mut res : Array String := #[]
    for _ in [0:t] do
      let n := toks.get! idx |>.toNat!
      idx := idx + 1
      let mut stops : Array (Nat × Nat) := #[]
      for _ in [0:n] do
        let d := toks[idx]!.toNat!
        let f := toks[idx+1]!.toNat!
        idx := idx + 2
        stops := stops.push (d,f)
      let L := toks[idx]!.toNat!
      let P := toks[idx+1]!.toNat!
      idx := idx + 2
      res := res.push (solveCase stops L P)
    return res

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let results := solve toks
  for line in results do
    IO.println line
