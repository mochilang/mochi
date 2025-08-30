-- https://www.spoj.com/problems/EXPRESS

import Std
open Std

inductive Node where
| leaf : Char -> Node
| op   : Char -> Node -> Node -> Node
deriving Inhabited

def Node.val : Node -> Char
| Node.leaf c => c
| Node.op c _ _ => c

partial def buildTree (s : String) : Node :=
  let rec loop (chars : List Char) (st : List Node) : Node :=
    match chars with
    | []      => st.head!
    | c :: cs =>
      if c.isLower then
        loop cs (Node.leaf c :: st)
      else
        match st with
        | r :: l :: rest => loop cs (Node.op c l r :: rest)
        | _ => Node.leaf 'x'
  loop s.data []

partial def queueExpr (s : String) : String :=
  let root := buildTree s
  let rec bfs (q : List Node) (acc : List Char) : List Char :=
    match q with
    | [] => acc
    | n :: rest =>
      let acc := n.val :: acc
      let rest :=
        match n with
        | Node.leaf _      => rest
        | Node.op _ l r => rest ++ [l, r]
      bfs rest acc
  String.mk (bfs [root] [])

partial def loop (h : IO.FS.Stream) (n : Nat) (acc : List String) : IO (List String) := do
  if n == 0 then
    pure acc
  else
    let line ← h.getLine
    loop h (n-1) (queueExpr line.trim :: acc)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let res ← loop h t []
  IO.println (String.intercalate "\n" res.reverse)
